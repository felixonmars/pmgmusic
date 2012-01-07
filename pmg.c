/*  PMG, a VOS clone for linux.
    Copyright (C) 2006 rainycat <rainy6144@gmail.com>, loveruby <loveruby.bbs@bbs.sjtu.edu.cn>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; see the file COPYING.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA. */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glade/glade.h>
#include <asoundlib.h>
#include <popt.h>
#include <openssl/evp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifndef SHA_DIGEST_LENGTH
#define SHA_DIGEST_LENGTH 20
#endif

#define IDX_NONE ((unsigned) -1)

#define NKEY 7

/* A note to be played by the user */
typedef struct usernote_s {
  double rt_start; /* Time of the Note-on event in seconds from the start of the song */
  double tt_start; /* Time of the Note-on event in MIDI quarter-notes from the start of the song */
  double rt_stop, tt_stop; /* For long notes, it is {rt,tt}_end; for short notes, it is {rt,tt}_start.
			      Used when displaying notes and scoring. */
  double rt_end, tt_end; /* Time of the corresponding Note-off event.  Used when playing short notes. */
  unsigned char cmd, note_num, vol; /* Corresponding MIDI command */
  unsigned key, color; /* Key is 0-6 for do,re,mi,fa,so,la,ti; Color is 0-15 */
  gboolean is_long;
  unsigned prev_idx, next_idx; /* The array index of the previous/next note on the same key;
				  IDX_NONE if there is none */
  unsigned prev_idx_long, next_idx_long;
  unsigned count, count_long; /* The number of (long) notes on this key, including this one */
} UserNote;

typedef struct inputevent_s {
  double rt;
  gboolean is_keypress;
  unsigned key;
} InputEvent;

typedef struct tempochange_s {
  double rt0, tt0; /* rt and tt at the time when the tempo change occurs */
  double qn_time; /* The duration of a MIDI quarter-note after that */
} TempoChange;

typedef struct midievent_s {
  double tt;
  unsigned char cmd, a, b; /* cmd is the status byte */
} MidiEvent;

/* Stores the MIDI initialization events in an MIDI track */
typedef struct miditrack_s {
  unsigned nevent;
  MidiEvent *events;
} MidiTrack;

/* Both BGM notes and notes played by the user */
typedef struct note_s {
  double tt_start, tt_end;
  double rt_start, rt_end;
  unsigned cmd, note_num, vol;
  gboolean is_user, is_long;
} Note;

typedef struct notearray_s {
  unsigned num_note;
  Note *notes;
} NoteArray;

/* An iterator in the TempoChange array, used to accelerate the converstion between rt's and tt's. */
typedef struct tempoit_s {
  unsigned idx; /* The index of the current TempoChange being used */
} TempoIt;

/* A scheduled event */
typedef enum schedeventtype_e {
  SchedEventMidi, SchedEventNoteOn, SchedEventNoteOff, SchedEventUserNoteOff, SchedEventDemo,
  SchedEventRefreshScore, SchedEventAutoOn, SchedEventAutoOff, SchedEventRefreshScreen
} SchedEventType;
typedef struct schedevent_s {
  double rt;
  SchedEventType type;
  unsigned src, idx; /* src is the track number or the note array number;
			idx is the index within the track or note array */
} SchedEvent;

typedef struct vossong_s {
  unsigned ntempo;
  TempoChange *tempos; /* The first TempoChange is the default one with qn_time=0.5 (120BPM) */
  double midi_unit_tt; /* The number of quarter-notes in a MIDI delta-time unit */
  unsigned ntrack;
  MidiTrack *tracks;
  unsigned num_note_array; /* Excluding the last one containing user notes */
  NoteArray *note_arrays;
  unsigned num_user_note;
  UserNote *user_notes;
  gboolean has_min_max_rt;
  double min_rt, max_rt; /* For now, they only count the notes, not the tracks */
  unsigned first_un_idxs[NKEY]; /* The first user note on each key */
  unsigned first_un_idxs_long[NKEY]; /* The first long user note */
  unsigned char hash[SHA_DIGEST_LENGTH];
} VosSong;

typedef struct comparerentry_s {
  unsigned last_idx; /* The index of the last user note used for matching */
  double score; /* Total score so far */
} ComparerEntry;

typedef struct comparer_s {
  double score0; /* Score when none of the user notes within range are used for matching */
  unsigned count0;
  unsigned nent;
  ComparerEntry *ents; /* Should be ordered so that both last_idx and score are strictly increasing */
} Comparer;

/* A marker is a circle showing the accuracy after a keypress */
typedef struct marker_s {
  double rt;
  double dt; /* dt = un->rt_start - cur_rt; dt > 0: too early; only the sign is used */
  double score;
} Marker;

typedef struct keystate_s {
  gboolean key_down; /* Whether the key on the keyboard is down or not */
  double key_down_rt;
  unsigned un_idx; /* For the actual note to play.
		      Only meaningful if key_down is true; IDX_NONE if the key_down had been ignored */
  Comparer cmp_press, cmp_release; /* One comparer for keypress events, another for key release events */
  double long_score, total_long_score;
  double long_rt; /* The score before long_rt has been counted into long_score and total_long_score. */
  gboolean long_okay, long_scored; /* The status at long_rt if key_down is true */
  GQueue *markers;
} KeyState;

typedef enum vosgamemode_e { VosGameModeNormal, VosGameModeAuto, VosGameModePlayDemo } VosGameMode;

typedef struct gamescore_s {
  double score, total_score;
} GameScore;

typedef struct gameoptions_s {
  VosGameMode mode;
  const char *vos_fname;
  int allow_cheating;
  int adaptive_time_factor;
  double target_ratio; /* For use with adaptive_time_factor */
  
  int half_volume_for_bgm;
  int fast_mode; /* In fast mode, we mainly just calculate the scores */
  int enable_ui, enable_sound;
  double fps;
  double igamma;
  
  double init_speed; /* vy per second of actual time */
  double seek;
  gboolean use_seek;
  double score_dt0; /* The amount of error, above which the lowest score is given; this is in actual time */
  /* A key can be held in the duration of a user note and plus allowed_hold_time on both sides.
     If it is held any longer, a penalty on long_score will be applied. */
  double allowed_hold_time; /* in actual time */
  int adjustable_mode; /* In adjustable mode, the time factor and the speed can be adjusted */
  double tempo_factor, init_time_factor;
  double pev_time_unit;

  const char *pev_in_fname;
  const char *pev_out_fname;
  const char *midi_port_spec;
  double score_refresh_period; /* In actual time */
  unsigned recent_scores_nperiod;
} GameOptions;

/* Options specified on the command line and shared by all VosGame instances */
static GameOptions defopts = {
  .mode = VosGameModeNormal,
  .vos_fname = NULL,
  .allow_cheating = FALSE,
  .adaptive_time_factor = FALSE,
  .target_ratio = 0.9,
  .half_volume_for_bgm = FALSE,
  .fast_mode = FALSE,
  .enable_ui = TRUE, .enable_sound = TRUE,
  .fps = 150.0, .igamma = 0.55,
  .init_speed = 1.4, .seek = 0, .use_seek = FALSE,
  .score_dt0 = 0.48, .allowed_hold_time = 0.20,
  .adjustable_mode = FALSE, .tempo_factor = 1.0, .init_time_factor = 1.0,
  .pev_time_unit = 0.001,
  .pev_in_fname = NULL, .pev_out_fname = NULL, .midi_port_spec = "128:0",
  .score_refresh_period = 0.25, .recent_scores_nperiod = 8
};

typedef struct vosgame_s {
  GameOptions *opts;
  GtkWidget *main_area;
  GtkLabel *state_label, *time_label, *score_label, *rate_label, *time_factor_label;
  VosSong *song;
  const char *user_name;
  snd_seq_t *seq;
  int seq_client_id, seq_port_id;
  int dest_client_id, dest_port_id;
  guint event_timeout_id;
  guint conn_key_press, conn_key_release, conn_expose, conn_delete;
  gboolean is_paused, is_resync;
  double start_rt; /* The rt at start_time */
  GTimeVal start_time, pause_time;
  double time_factor, speed_rt, tscale_rt; /* speed_rt does not change automatically during game */
  double frame_time; /* 1.0 / fps */
  double volume;
  GArray *input_events_arr; /* Used when recording demos */
  const InputEvent *in_evs; /* Used when playing demos */
  unsigned in_nev;
  double last_status_upd_rt;
  GameScore *recent_scores;
  GTree *sched; /* event scheduler */
  GdkColor *colors;
  TempoIt it;
  unsigned cur_un_idxs[NKEY]; /* Used for speeding up the search in the user notes array;
				 Always points to a note on that key, unless there is no note on this key,
				 in which case the value is IDX_NONE */
  unsigned cur_un_idxs_long[NKEY];
  KeyState keys[NKEY];
} VosGame;

/* Information in the header of input events */
#define PEV_MAGIC 0xb10cde81U
/* Header types */
#define PEV_HEADER_END 0
#define PEV_HASH_SHA1 1
#define PEV_TEMPO_FACTOR 2 /* Skipped if this is 1 */
#define PEV_SPEED 3 /* NOTE: this might be bogus if speed can be changed in-game */
#define PEV_TIME_UNIT 4
#define PEV_DATE 5
#define PEV_USER_NAME 6
#define PEV_TIME_OFFSET 7
#define PEV_TIME_FACTOR 8
#define PEV_SEEK 9
#define PEV_CHEAT 10
#define PEV_ADAPTIVE_TIME_FACTOR 11
/* Event types */
#define PEV_EV_END 0
#define PEV_EV_PRESS 1
#define PEV_EV_RELEASE 8 /* The next event type should be 15 */

static const struct poptOption popt_table[] = {
  { "port", 'p', POPT_ARG_STRING | POPT_ARGFLAG_SHOW_DEFAULT, &defopts.midi_port_spec, 0,
    "Set the MIDI sequencer port", "CLIENT:PORT" },
  { "speed", 's', POPT_ARG_DOUBLE | POPT_ARGFLAG_SHOW_DEFAULT, &defopts.init_speed, 0,
    "Set the speed of the notes falling down", "SPEED" },
  { "seek", 'k', POPT_ARG_DOUBLE, &defopts.seek, 'k', "Seek to some time at startup", "RT" },
  { "cheat", 'c', POPT_ARG_NONE, &defopts.allow_cheating, 0, "Allow in-game changes to the time factor", NULL },
  { "dt0", 'd', POPT_ARG_DOUBLE | POPT_ARGFLAG_SHOW_DEFAULT, &defopts.score_dt0, 0,
    "Set the maximum allowed error in seconds", "TIME" },
  { "tempo", 'T', POPT_ARG_DOUBLE, &defopts.tempo_factor, 0, "Multiply the tempo by FACTOR", "FACTOR" },
  { "time-factor", 't', POPT_ARG_DOUBLE, &defopts.init_time_factor, 0, "Make time pass FACTOR times as quickly", "FACTOR" },
  { "adaptive-tf", 'a', POPT_ARG_DOUBLE, &defopts.target_ratio, 'a', "Adaptively adjust the time factor", "TARGET_RATIO" },
  { "record", 'o', POPT_ARG_STRING, &defopts.pev_out_fname, 0, "Record into demo file FNAME.pev", "FNAME.pev" },
  { "playdemo", 'i', POPT_ARG_STRING, &defopts.pev_in_fname, 'i', "Play demo file FNAME.pev", "FNAME.pev" },
  { "fast", 'f', POPT_ARG_NONE, &defopts.fast_mode, 0, "Fast mode", NULL },
  { "sound-only", 'S', POPT_ARG_NONE, NULL, 'S', "Play sound only, no graphics", NULL },
  { "auto", 'A', POPT_ARG_NONE, NULL, 'A', "Automatic mode", NULL },
  { "verbose", 'v', POPT_ARG_NONE, NULL, 'v', "Verbose", NULL },
  POPT_AUTOHELP
  POPT_TABLEEND };

#define GET_WIDGET(gx, obj, name, type)			\
do {							\
  (obj) = type(glade_xml_get_widget((gx), (name)));	\
  g_assert((obj) != NULL);				\
 } while (0)

#define COLOR_BLACK 0
#define COLOR_WHITE 1
#define COLOR_KEYPRESS_START 2
#define COLOR_KEYPRESS 10
#define COLOR_NOTE_START 11
#define COLOR_NOTE_END 42
#define COLOR_MARKER_START 43
#define COLOR_MARKER_BEST 43
#define COLOR_MARKER_WORST 59
#define COLOR_MARKER_END 74
#define COLOR_RATE_START 75
#define COLOR_RATE_END 106

#define NCOLOR 107

static const char *glade_fname = "pmg.glade";
static const double vx_min = -0.1, vx_max = 7.1, vx_mid = 3.5, vy_min = -1.0, vy_max = 0.2;
static const double note_vw = 0.1; /* width of long note borders */
static const double note_vt = 0.01;

/* Whether the length of short notes are automatically lengthened to the actual note length */
static const gboolean short_note_auto_len = TRUE;

gboolean verbose = FALSE;

/* Returns t-t0 in seconds */
static double timeval_diff(GTimeVal *t0, GTimeVal *t)
{
  return (t->tv_sec - t0->tv_sec) + 1e-6 * (t->tv_usec - t0->tv_usec);
}

#define BUF_SIZE 4096
static void hash_file(FILE *file, unsigned char *hash, unsigned hash_len)
{
  static char buf[BUF_SIZE];
  unsigned cur_len;
  EVP_MD_CTX mdctx;
  const EVP_MD *md = EVP_sha1();
  int result;

  g_assert(EVP_MD_size(md) == (int) hash_len);
  result = EVP_DigestInit(&mdctx, md); g_assert(result == 1);
  result = fseek(file, 0, SEEK_SET); g_assert(result == 0);
  while (1) {
    result = fread(buf, 1, BUF_SIZE, file); g_assert(result >= 0); cur_len = result;
    if (cur_len == 0) break;
    result = EVP_DigestUpdate(&mdctx, buf, cur_len); g_assert(result == 1);
  }
  result = EVP_DigestFinal(&mdctx, hash, NULL); g_assert(result == 1);
}
#undef BUF_SIZE

static void tempoit_init(VosSong *song, TempoIt *it)
{
  it->idx = 0;
}

static double tempoit_tt_to_rt(VosSong *song, TempoIt *it, double tt)
{
  const TempoChange *tc;
  
  while (it->idx > 0 && tt < song->tempos[it->idx].tt0) it->idx--;
  while (it->idx + 1 < song->ntempo && tt >= song->tempos[it->idx+1].tt0) it->idx++;
  tc = &song->tempos[it->idx];
  return tc->rt0 + (tt - tc->tt0) * tc->qn_time;
}

static double tempoit_rt_to_tt(VosSong *song, TempoIt *it, double rt)
{
  const TempoChange *tc;

  while (it->idx > 0 && rt < song->tempos[it->idx].rt0) it->idx--;
  while (it->idx + 1 < song->ntempo && rt >= song->tempos[it->idx+1].rt0) it->idx++;
  tc = &song->tempos[it->idx];
  return tc->tt0 + (rt - tc->rt0) / tc->qn_time;
}

static gint event_compare(gconstpointer pa, gconstpointer pb, gpointer user_data)
{
  const SchedEvent *a = (SchedEvent *) pa, *b = (SchedEvent *) pb;
  if (a->rt != b->rt) return (a->rt > b->rt) - (a->rt < b->rt);
  else return (a > b) - (a < b);
}

static GTree *new_event_scheduler(void)
{
  return g_tree_new_full(event_compare, NULL, NULL, g_free);
}

static void free_event_scheduler(GTree *sched)
{
  g_tree_destroy(sched);
}

static void sched_event(GTree *sched, double rt, SchedEventType type, unsigned src, unsigned idx)
{
  SchedEvent *ev = g_new0(SchedEvent, 1);
  ev->rt = rt; ev->type = type; ev->src = src; ev->idx = idx;
  g_tree_insert(sched, ev, ev);
}

static gboolean get_first_event_each(gpointer key, gpointer value, gpointer data)
{
  SchedEvent **pev = (SchedEvent **) data;
  *pev = (SchedEvent *) key;
  return TRUE; /* stop traversal */
}

static SchedEvent *get_first_event(GTree *sched)
{
  SchedEvent *ev = NULL;
  g_tree_foreach(sched, get_first_event_each, &ev);
  return ev;
}

static void remove_event(GTree *sched, SchedEvent *ev)
{
  g_tree_remove(sched, ev);
}

static void free_vos_song(VosSong *song)
{
  unsigned i;
  
  g_free(song->tempos);
  for (i = 0; i < song->ntrack; i++) g_free(song->tracks[i].events);
  g_free(song->tracks);
  for (i = 0; i < song->num_note_array; i++) g_free(song->note_arrays[i].notes);
  g_free(song->note_arrays);
  g_free(song->user_notes);
  g_free(song);
}

static unsigned midi_read_u8(FILE *file)
{
  int result = getc(file);
  g_assert(result != EOF);
  return (unsigned) result;
}

static unsigned midi_read_u16(FILE *file)
{
  guint16 tmp;
  int result;

  result = fread(&tmp, 2, 1, file); g_assert(result == 1);
  return GUINT16_FROM_BE(tmp);
}

static unsigned midi_read_u32(FILE *file)
{
  guint32 tmp;
  int result;

  result = fread(&tmp, 4, 1, file); g_assert(result == 1);
  return GUINT32_FROM_BE(tmp);
}

static unsigned midi_read_vl(FILE *file)
{
  int result;
  unsigned x = 0;

  while (1) {
    result = getc(file); g_assert(result != EOF);
    x |= (result & 0x7f);
    if (result & 0x80) x <<= 7;
    else break;
  }
  return x;
}

static int tempo_change_compare(gconstpointer pa, gconstpointer pb)
{
  const TempoChange *a = (const TempoChange *) pa, *b = (const TempoChange *) pb;
  return (a->tt0 > b->tt0) - (a->tt0 < b->tt0);
}

static void vosfile_read_midi(VosSong *song, FILE *file, unsigned mid_ofs, unsigned mid_len, double tempo_factor)
{
  int result;
  char magic[4];
  unsigned header_len, fmt, ppqn, i;
  GArray *tempo_arr;
  TempoChange tc;
  
  result = fseek(file, mid_ofs, SEEK_SET); g_assert(result == 0);
  result = fread(magic, 4, 1, file); g_assert(result == 1);
  g_assert(memcmp(magic, "MThd", 4) == 0);
  header_len = midi_read_u32(file); g_assert(header_len == 6);
  fmt = midi_read_u16(file); g_assert(fmt == 1);
  song->ntrack = midi_read_u16(file); g_assert(song->ntrack > 0);
  ppqn = midi_read_u16(file); g_assert((ppqn & 0x8000) == 0); /* Actually a ppqn */
  song->midi_unit_tt = 1.0 / ppqn;

  song->tracks = g_new0(MidiTrack, song->ntrack);
  tempo_arr = g_array_new(FALSE, FALSE, sizeof(TempoChange));
  tc.rt0 = 0.0; tc.tt0 = 0.0; tc.qn_time = 0.5; g_array_append_val(tempo_arr, tc); /* 120 BPM, the default */
  for (i = 0; i < song->ntrack; i++) {
    MidiTrack *track = &song->tracks[i];
    GArray *event_arr = g_array_new(FALSE, FALSE, sizeof(MidiEvent));
    unsigned track_ofs, track_len;
    unsigned char cmd = 0; /* The status byte */
    unsigned char ch;
    unsigned time = 0; /* In MIDI units */
    gboolean is_eot = FALSE; /* Found "End of Track"? */

    result = fread(magic, 4, 1, file); g_assert(result == 1);
    g_assert(memcmp(magic, "MTrk", 4) == 0);
    track_len = midi_read_u32(file);
    track_ofs = ftell(file);
    while ((unsigned) ftell(file) < track_ofs + track_len) {
      MidiEvent ev;
      double tt;
      
      g_assert(! is_eot);
      memset(&ev, 0, sizeof(ev));
      time += midi_read_vl(file); tt = time * song->midi_unit_tt;
      ch = midi_read_u8(file);
      if (ch == 0xff) { /* Meta event */
	unsigned meta_type = midi_read_u8(file);
	unsigned meta_len = midi_read_vl(file);
	switch (meta_type) {
	default:
	  g_warning("Unknown meta event type 0x%02x", meta_type);
	  /* fall through */
	case 0x03: /* Sequence/Track name */
	case 0x21: /* MIDI port number */
	case 0x58: /* Time signature */
	case 0x59: /* Key signature */
	  result = fseek(file, meta_len, SEEK_CUR); g_assert(result == 0); /* skip over the data */
	  break;
	case 0x2f: /* End of track */
	  g_assert(meta_len == 0); is_eot = TRUE; break;
	case 0x51: /* Tempo */
	  {
	    unsigned char buf[3];
	    unsigned val;

	    /* NOTE: If tempo changes exist in multiple tracks (e.g. 2317.vos), the array will be out of order.  Therefore,
	       we set rt0 only after sorting them. */
	    g_assert(meta_len == 3);
	    result = fread(buf, meta_len, 1, file); g_assert(result == 1);
	    val = ((unsigned) buf[0] << 16) | ((unsigned) buf[1] << 8) | buf[2];
	    tc.tt0 = tt; tc.rt0 = 0.0; tc.qn_time = val * 1e-6 / tempo_factor; g_array_append_val(tempo_arr, tc);
	  }
	  break;
	}
	continue;
      } else if (ch == 0xf0) { /* SysEx event */
	unsigned sysex_len = midi_read_vl(file);
	/* Just ignore it */
	result = fseek(file, sysex_len, SEEK_CUR); g_assert(result == 0);
      } else {
	unsigned cmd_type;
	if (ch & 0x80) { cmd = ch; ch = midi_read_u8(file); }
	cmd_type = (cmd & 0x7f) >> 4; g_assert(cmd_type != 7);
	ev.cmd = cmd; ev.a = ch;
	/* Program Change and Channel Pressure messages have one status bytes, all others have two */
	if (! (cmd_type == 4 || cmd_type == 5)) ev.b = midi_read_u8(file);
      }
      ev.tt = tt;
      g_array_append_val(event_arr, ev);
    }
    g_assert(is_eot);
    g_assert((unsigned) ftell(file) == track_ofs + track_len);
    track->nevent = event_arr->len;
    track->events = (MidiEvent *) g_array_free(event_arr, FALSE);
  }
  g_array_sort(tempo_arr, tempo_change_compare);
  song->ntempo = tempo_arr->len;
  song->tempos = (TempoChange *) g_array_free(tempo_arr, FALSE);
  for (i = 1; i < song->ntempo; i++) { /* song->tempos[0].rt0 has been set to zero */
    const TempoChange *last_tc = &song->tempos[i-1];
    TempoChange *tc = &song->tempos[i];
    tc->rt0 = last_tc->rt0 + last_tc->qn_time * (tc->tt0 - last_tc->tt0);
  }
  if (verbose) {
    for (i = 0; i < song->ntempo; i++) {
      const TempoChange *tc = &song->tempos[i];
      int rtt = (int) tc->rt0;
      g_message("Tempo at %d:%02d: %d bpm", rtt / 60, rtt % 60, (int) floor(60.0 / tc->qn_time + 0.5));
    }
  }
}

static unsigned vosfile_read_u8(FILE *file)
{
  int result = getc(file);

  g_assert(result != EOF);
  return (unsigned) result;
}

/* Little endian */
static unsigned vosfile_read_u16(FILE *file)
{
  guint16 tmp;
  int result;

  result = fread(&tmp, 2, 1, file); g_assert(result == 1);
  return GUINT16_FROM_LE(tmp);
}

static unsigned vosfile_read_u32(FILE *file)
{
  guint32 tmp;
  int result;

  result = fread(&tmp, 4, 1, file); g_assert(result == 1);
  return GUINT32_FROM_LE(tmp);
}

static char *vosfile_read_string(FILE *file)
{
  unsigned len;
  char *buf;
  int result;

  len = vosfile_read_u8(file); buf = g_new0(char, len + 1);
  result = fread(buf, 1, len, file); g_assert(result == (int) len);
  return buf;
}

static char *vosfile_read_string2(FILE *file)
{
  unsigned len;
  char *buf;
  int result;

  len = vosfile_read_u16(file); buf = g_new0(char, len + 1);
  result = fread(buf, 1, len, file); g_assert(result == (int) len);
  return buf;
}

static char *vosfile_read_string_fixed(FILE *file, unsigned len)
{
  char *buf;
  int result;
  
  buf = g_new0(char, len + 1);
  result = fread(buf, 1, len, file); g_assert(result == (int) len);
  return buf;
}

static void print_vosfile_string(const char *name, const char *raw_str)
{
  char *str = g_convert(raw_str, -1, "UTF-8", "GB2312", NULL, NULL, NULL);
  if (str == NULL || *str != '\0') g_message("%s: %s", name, (str != NULL) ? str : "[conversion failed]");
  if (str) g_free(str);
}

static void print_vosfile_string_v(const char *name, const char *raw_str)
{
  if (verbose) print_vosfile_string(name, raw_str);
}

static void vosfile_read_info(VosSong *song, FILE *file, unsigned inf_ofs, unsigned inf_len)
{
  int result;
  unsigned inf_end_ofs = inf_ofs + inf_len, cur_ofs, next_ofs, key;
  char *title, *artist, *comment, *vos_author;
  unsigned song_type, ext_type, song_length, level;
  char buf[4];
  GArray *note_arrays_arr = g_array_new(FALSE, FALSE, sizeof(NoteArray));
  GArray *user_notes_arr = g_array_new(FALSE, FALSE, sizeof(UserNote));
  unsigned last_note_idx[NKEY], last_note_idx_long[NKEY], cur_un_idx;
  unsigned count[NKEY], count_long[NKEY];

  result = fseek(file, inf_ofs, SEEK_SET); g_assert(result == 0);
  /* Skip the "VOS1" header in e.g. test3.vos and test4.vos, if any */
  result = fread(buf, 4, 1, file);
  if (result == 1 && memcmp(buf, "VOS1", 4) == 0) { /* Found "VOS1" header */
    char *str1;
    result = fseek(file, 66, SEEK_CUR); g_assert(result == 0);
    str1 = vosfile_read_string(file); g_free(str1);
  } else {
    result = fseek(file, inf_ofs, SEEK_SET); g_assert(result == 0);
  }
  title = vosfile_read_string(file); print_vosfile_string_v("Title", title); g_free(title);
  artist = vosfile_read_string(file); print_vosfile_string_v("Artist", artist); g_free(artist);
  comment = vosfile_read_string(file); print_vosfile_string_v("Comment", comment); g_free(comment);
  vos_author = vosfile_read_string(file); print_vosfile_string_v("VOS Author", vos_author); g_free(vos_author);
  song_type = vosfile_read_u8(file); ext_type = vosfile_read_u8(file);
  song_length = vosfile_read_u32(file);
  level = vosfile_read_u8(file); if (verbose) g_message("Level: %u", level + 1);

  result = fseek(file, 1023, SEEK_CUR); g_assert(result == 0);
  result = ftell(file); g_assert(result != -1); cur_ofs = result;
  for (key = 0; key < NKEY; key++) {
    count[key] = 0; count_long[key] = 0;
    last_note_idx[key] = IDX_NONE; last_note_idx_long[key] = IDX_NONE;
    song->first_un_idxs[key] = IDX_NONE; song->first_un_idxs_long[key] = IDX_NONE;
  }
  cur_un_idx = 0;
  while (1) {
    unsigned type, nnote, i;
    gboolean is_user_arr; /* Whether the current note array is the one to be played by the user. */
    char dummy2[14];
    NoteArray cur_note_arr;
    TempoIt it;

    result = ftell(file); g_assert(result != -1); cur_ofs = result;
    if (cur_ofs == inf_end_ofs) break;
    type = vosfile_read_u32(file); nnote = vosfile_read_u32(file);
    next_ofs = cur_ofs + nnote * 13 + 22; is_user_arr = (next_ofs == inf_end_ofs);
    result = fread(dummy2, 14, 1, file); g_assert(result == 1);
    for (i = 0; i < 14; i++) g_assert(dummy2[i] == 0);

    memset(&cur_note_arr, 0, sizeof(cur_note_arr));
    tempoit_init(song, &it);
    if (! is_user_arr) { cur_note_arr.num_note = nnote; cur_note_arr.notes = g_new0(Note, nnote); }
    for (i = 0; i < nnote; i++) {
      unsigned time, len;
      unsigned char cmd, note_num, vol;
      unsigned flags;
      gboolean is_user_note, is_long;
      double tt_start, tt_end, rt_start, rt_end;
      
      time = vosfile_read_u32(file); len = vosfile_read_u32(file);
      cmd = vosfile_read_u8(file); note_num = vosfile_read_u8(file); vol = vosfile_read_u8(file);
      flags = vosfile_read_u16(file); is_user_note = ((flags & 0x80) != 0); is_long = ((flags & 0x8000) != 0);

      tt_start = time / 768.0; tt_end = (time + len) / 768.0;
      rt_start = tempoit_tt_to_rt(song, &it, tt_start); rt_end = tempoit_tt_to_rt(song, &it, tt_end);
      if (song->has_min_max_rt) { song->min_rt = MIN(song->min_rt, rt_start); song->max_rt = MAX(song->max_rt, rt_end); }
      else { song->min_rt = rt_start; song->max_rt = rt_end; song->has_min_max_rt = TRUE; }
      if (! is_user_arr) {
	Note *note = &cur_note_arr.notes[i];
	note->tt_start = tt_start; note->tt_end = tt_end;
	note->rt_start = rt_start; note->rt_end = rt_end;
	note->cmd = cmd; note->note_num = note_num; note->vol = vol;
	note->is_user = is_user_note; note->is_long = is_long;
      } else {
	UserNote un;
	
	g_assert(is_user_note);
	memset(&un, 0, sizeof(un));
	un.tt_start = tt_start; un.rt_start = rt_start;
	un.tt_end = tt_end; un.rt_end = rt_end;
	un.cmd = cmd; un.note_num = note_num; un.vol = vol;
	un.key = (flags & 0x70) >> 4; un.color = (flags & 0x0f); un.is_long = is_long;
	un.tt_stop = (un.is_long) ? un.tt_end : un.tt_start;
	un.rt_stop = (un.is_long) ? un.rt_end : un.rt_start;
	g_assert(un.key < NKEY);
	un.prev_idx = last_note_idx[un.key]; un.next_idx = IDX_NONE;
	if (last_note_idx[un.key] != IDX_NONE) {
	  UserNote *last_un = &g_array_index(user_notes_arr, UserNote, last_note_idx[un.key]);
	  if (un.tt_start < last_un->tt_stop) { /* Notes on the same key should never overlap */
	    g_warning("User note %u ignored because of overlapping notes.", i);
	    goto skip_user_note;
	  }
	  last_un->next_idx = cur_un_idx;
	} else song->first_un_idxs[un.key] = cur_un_idx; /* First note on this key */
	last_note_idx[un.key] = cur_un_idx;
	un.count = ++count[un.key];
	if (un.is_long) {
	  un.prev_idx_long = last_note_idx_long[un.key]; un.next_idx_long = IDX_NONE;
	  if (last_note_idx_long[un.key] != IDX_NONE)
	    g_array_index(user_notes_arr, UserNote, last_note_idx_long[un.key]).next_idx_long = cur_un_idx;
	  else song->first_un_idxs_long[un.key] = cur_un_idx;
	  last_note_idx_long[un.key] = cur_un_idx;
	  un.count_long = ++count_long[un.key];
	} else { un.prev_idx_long = IDX_NONE; un.next_idx_long = IDX_NONE; }
	g_array_append_val(user_notes_arr, un); cur_un_idx++;
      skip_user_note: ;
      }
    }
    if (! is_user_arr) g_array_append_val(note_arrays_arr, cur_note_arr);
    cur_ofs = next_ofs; g_assert((unsigned) ftell(file) == cur_ofs);
  }
  song->num_note_array = note_arrays_arr->len;
  song->note_arrays = (NoteArray *) g_array_free(note_arrays_arr, FALSE);
  song->num_user_note = user_notes_arr->len;
  song->user_notes = (UserNote *) g_array_free(user_notes_arr, FALSE);
}

static void vosfile_read_info_022(VosSong *song, FILE *file, unsigned inf_ofs, unsigned inf_len)
{
  int result;
  char *title, *artist, *comment, *vos_author, *str;
  unsigned version;
  unsigned song_length, level, x;
  unsigned i, j, k, key;
  unsigned narr, nunote; /* NOTE: due to the deletion of buggy notes, song->num_user_note may be smaller than nunote */
  char magic[6], unknown1[11];
  GArray *user_notes_arr = g_array_new(FALSE, FALSE, sizeof(UserNote));
  unsigned last_note_idx[NKEY], last_note_idx_long[NKEY], cur_un_idx;
  unsigned count[NKEY], count_long[NKEY];

  result = fseek(file, inf_ofs, SEEK_SET); g_assert(result == 0);
  result = fread(magic, 6, 1, file); g_assert(result == 1);
  if (memcmp(magic, "VOS022", 6) == 0) version = 22;
  else if (memcmp(magic, "VOS006", 6) == 0) version = 6;
  else g_assert_not_reached();

  title = vosfile_read_string2(file); print_vosfile_string_v("Title", title); g_free(title);
  artist = vosfile_read_string2(file); print_vosfile_string_v("Artist", artist); g_free(artist);
  comment = vosfile_read_string2(file); print_vosfile_string_v("Comment", comment); g_free(comment);
  vos_author = vosfile_read_string2(file); print_vosfile_string_v("VOS Author", vos_author); g_free(vos_author);
  str = vosfile_read_string2(file); g_free(str);
  result = fread(unknown1, 11, 1, file); g_assert(result == 1);
  x = vosfile_read_u32(file); /* song_length_tt? */ song_length = vosfile_read_u32(file);

  result = fseek(file, 1024, SEEK_CUR); g_assert(result == 0);
  narr = vosfile_read_u32(file); x = vosfile_read_u32(file); g_assert(x == 1);
  song->num_note_array = narr; song->note_arrays = g_new0(NoteArray, narr);
  for (k = 0; k < narr; k++) {
    x = vosfile_read_u8(file); g_assert(x == 4);
    x = vosfile_read_u32(file); /* type */
  }
  x = vosfile_read_u8(file); g_assert(x == 0);
  level = vosfile_read_u8(file); if (verbose) g_message("Level: %u", level + 1);
  str = vosfile_read_string2(file); g_free(str);
  x = vosfile_read_u32(file); g_assert(x == 0);

  for (key = 0; key < NKEY; key++) {
    count[key] = 0; count_long[key] = 0;
    last_note_idx[key] = IDX_NONE; last_note_idx_long[key] = IDX_NONE;
    song->first_un_idxs[key] = IDX_NONE; song->first_un_idxs_long[key] = IDX_NONE;
  }
  cur_un_idx = 0;

  /* Notes */
  for (k = 0; k < narr; k++) {
    unsigned nnote;
    TempoIt it;
    NoteArray *cur_note_arr = &song->note_arrays[k];

    nnote = vosfile_read_u32(file);
    cur_note_arr->num_note = nnote; cur_note_arr->notes = g_new0(Note, nnote);
    tempoit_init(song, &it);
    for (i = 0; i < nnote; i++) {
      unsigned time, len;
      unsigned char cmd, note_num, vol;
      unsigned track;
      gboolean is_user_note, is_long;
      double tt_start, tt_end, rt_start, rt_end;
      Note *note = &cur_note_arr->notes[i];

      x = vosfile_read_u8(file); g_assert(x == 0);
      time = vosfile_read_u32(file); note_num = vosfile_read_u8(file);
      track = vosfile_read_u8(file); cmd = track | 0x90;
      vol = vosfile_read_u8(file); is_user_note = vosfile_read_u8(file);
      x = vosfile_read_u8(file);
#if 0
      if (x != 1) g_message("Special note: k=%u i=%u x=%u", k, i, x);
#endif
      is_long = vosfile_read_u8(file); len = vosfile_read_u32(file);
      x = vosfile_read_u8(file); g_assert(x == 0x00 || x == 0xff);
#if 0
      /* This is true for most files, but 994.vos is an exception */
      g_assert(x == (is_user_note ? 0x00 : 0xff));
#endif
      tt_start = time / 768.0; tt_end = (time + len) / 768.0;
      rt_start = tempoit_tt_to_rt(song, &it, tt_start); rt_end = tempoit_tt_to_rt(song, &it, tt_end);
      if (song->has_min_max_rt) { song->min_rt = MIN(song->min_rt, rt_start); song->max_rt = MAX(song->max_rt, rt_end); }
      else { song->min_rt = rt_start; song->max_rt = rt_end; song->has_min_max_rt = TRUE; }
      
      note->tt_start = tt_start; note->tt_end = tt_end;
      note->rt_start = rt_start; note->rt_end = rt_end;
      note->cmd = cmd; note->note_num = note_num; note->vol = vol;
      note->is_user = is_user_note; note->is_long = is_long;
    }
  }

  /* User notes */
  if (version == 22) { x = vosfile_read_u32(file); g_assert(x == 0); }
  nunote = vosfile_read_u32(file);
  for (j = 0; j < nunote; j++) {
    NoteArray *cur_note_arr;
    Note *note;
    UserNote un;
    
    k = vosfile_read_u8(file); g_assert(k < narr); cur_note_arr = &song->note_arrays[k];
    i = vosfile_read_u32(file); g_assert(i < cur_note_arr->num_note); note = &cur_note_arr->notes[i];
    key = vosfile_read_u8(file);
    
    g_assert(note->is_user);
    memset(&un, 0, sizeof(un));
    un.tt_start = note->tt_start; un.rt_start = note->rt_start;
    un.tt_end = note->tt_end; un.rt_end = note->rt_end;
    un.cmd = note->cmd; un.note_num = note->note_num; un.vol = note->vol;
    un.key = key; un.color = (k & 0x0f); un.is_long = note->is_long;
    un.tt_stop = (un.is_long) ? un.tt_end : un.tt_start;
    un.rt_stop = (un.is_long) ? un.rt_end : un.rt_start;
    g_assert(un.key < NKEY);
    un.prev_idx = last_note_idx[un.key]; un.next_idx = IDX_NONE;
    if (last_note_idx[un.key] != IDX_NONE) {
      UserNote *last_un = &g_array_index(user_notes_arr, UserNote, last_note_idx[un.key]);
      if (un.tt_start < last_un->tt_stop) { /* Notes on the same key should never overlap */
	g_warning("User note %u ignored because of overlapping notes.", i);
	goto skip_user_note;
      }
      last_un->next_idx = cur_un_idx;
    } else song->first_un_idxs[un.key] = cur_un_idx; /* First note on this key */
    last_note_idx[un.key] = cur_un_idx;
    un.count = ++count[un.key];
    if (un.is_long) {
      un.prev_idx_long = last_note_idx_long[un.key]; un.next_idx_long = IDX_NONE;
      if (last_note_idx_long[un.key] != IDX_NONE)
	g_array_index(user_notes_arr, UserNote, last_note_idx_long[un.key]).next_idx_long = cur_un_idx;
      else song->first_un_idxs_long[un.key] = cur_un_idx;
      last_note_idx_long[un.key] = cur_un_idx;
      un.count_long = ++count_long[un.key];
    } else { un.prev_idx_long = IDX_NONE; un.next_idx_long = IDX_NONE; }
    g_array_append_val(user_notes_arr, un); cur_un_idx++;
  skip_user_note: ;    
  }
  song->num_user_note = user_notes_arr->len;
  song->user_notes = (UserNote *) g_array_free(user_notes_arr, FALSE);

  /* What follows is the lyric, which we ignore for now. */
}

static void vosfile_read_vos1(VosSong *song, FILE *file, unsigned file_size, double tempo_factor)
{
  unsigned ofs, next_ofs;
  unsigned inf_ofs = 0, inf_len = 0, mid_ofs = 0, mid_len = 0;
  char *seg_name;
  
  /* Read the segments */
  ofs = vosfile_read_u32(file);
  while (1) {
    seg_name = vosfile_read_string_fixed(file, 16);
    if (strcmp(seg_name, "EOF") == 0 || ofs == file_size) { g_free(seg_name); break; }
    next_ofs = vosfile_read_u32(file);
    if (strcmp(seg_name, "inf") == 0) { inf_ofs = ofs; inf_len = next_ofs - ofs; }
    else if (strcmp(seg_name, "mid") == 0) { mid_ofs = ofs; mid_len = next_ofs - ofs; }
    else g_assert_not_reached();
    g_free(seg_name);
    ofs = next_ofs;
  }
  g_assert(inf_len != 0); g_assert(mid_len != 0);

  vosfile_read_midi(song, file, mid_ofs, mid_len, tempo_factor);
  vosfile_read_info(song, file, inf_ofs, inf_len);
}

static void vosfile_read_vos022(VosSong *song, FILE *file, unsigned file_size, double tempo_factor)
{
  int result;
  unsigned inf_ofs = IDX_NONE, inf_len = 0, mid_ofs = IDX_NONE, mid_len = 0;
  unsigned subfile_idx = 0, ofs = 4, data_ofs;
  unsigned fname_len, len;
  char *fname;

  while (1) {
    if (ofs == file_size) break;
    result = fseek(file, ofs, SEEK_SET); g_assert(result == 0);
    fname_len = vosfile_read_u32(file);
    fname = vosfile_read_string_fixed(file, fname_len);
    len = vosfile_read_u32(file); data_ofs = ofs + 4 + fname_len + 4;
    if (subfile_idx == 0) { g_assert(strcmp(fname, "Vosctemp.trk") == 0); inf_ofs = data_ofs; inf_len = len; }
    else if (subfile_idx == 1) {
      g_assert(strcmp(fname, "VOSCTEMP.mid") == 0); mid_ofs = data_ofs, mid_len = len;
    } else g_assert_not_reached();
    g_free(fname);
    ofs = data_ofs + len; subfile_idx++;
  }
  g_assert(inf_ofs != IDX_NONE); g_assert(mid_ofs != IDX_NONE);

  vosfile_read_midi(song, file, mid_ofs, mid_len, tempo_factor);
  vosfile_read_info_022(song, file, inf_ofs, inf_len);
}

static VosSong *read_vos_file(const char *fname, double tempo_factor)
{
  VosSong *song;
  FILE *file;
  unsigned magic, file_size;
  int result;
  
  file = fopen(fname, "rb"); g_assert(file != NULL);
  song = g_new0(VosSong, 1); song->has_min_max_rt = FALSE;
  hash_file(file, song->hash, SHA_DIGEST_LENGTH);
  result = fseek(file, 0, SEEK_END); g_assert(result == 0);
  file_size = ftell(file);
  result = fseek(file, 0, SEEK_SET); g_assert(result == 0);
  magic = vosfile_read_u32(file);
  if (magic == 3) vosfile_read_vos1(song, file, file_size, tempo_factor);
  else if (magic == 2) vosfile_read_vos022(song, file, file_size, tempo_factor);
  else g_assert_not_reached();
  
  fclose(file);
  return song;
}

#define SET_COLOR(idx, r, g, b)					\
do {								\
  game->colors[(idx)].red = (guint16) ((double) (r) * 65535);	\
  game->colors[(idx)].green = (guint16) ((double) (g) * 65535);	\
  game->colors[(idx)].blue = (guint16) ((double) (b) * 65535);	\
 } while (0)

/* h is in 0..3 */
static void hsv2rgb(VosGame *game, double h, double s, double v, double *pr, double *pg, double *pb)
{
  double igamma = game->opts->igamma;
  double x0 = (1.0 - s) * v, dx = s * v;
  double r, g, b;

  if (h < 1.0) { r = x0 + (1.0 - h) * dx; g = x0 + h * dx; b = x0; }
  else if (h < 2.0) { r = x0; g = x0 + (2.0 - h) * dx; b = x0 + (h - 1.0) * dx; }
  else { r = x0 + (h - 2.0) * dx; g = x0; b = x0 + (3.0 - h) * dx; }
  r = pow(r, igamma); g = pow(g, igamma); b = pow(b, igamma);
  *pr = r; *pg = g; *pb = b;
}

static void init_colors(VosGame *game)
{
  gboolean *success = g_newa(gboolean, NCOLOR);
  unsigned i;

  game->colors = g_new0(GdkColor, NCOLOR);
  SET_COLOR(COLOR_BLACK, 0.0, 0.0, 0.0);
  SET_COLOR(COLOR_WHITE, 1.0, 1.0, 1.0);
  for (i = COLOR_KEYPRESS_START; i <= COLOR_KEYPRESS; i++) SET_COLOR(i, 0.15, 0.15, 0.15);
  g_assert(COLOR_NOTE_END == COLOR_NOTE_START + 31);
  for (i = 0; i < 16; i++) {
    unsigned j = (i * 5) % 16;
    double r, g, b;
    if (j == 0) r = g = b = 0.9;
    else {
      double h = (j - 1.0) * 0.2;
      hsv2rgb(game, h, 0.5, 1.0, &r, &g, &b);
    }
    SET_COLOR(COLOR_NOTE_START + i * 2, r, g, b);
    SET_COLOR(COLOR_NOTE_START + i * 2 + 1, r * 0.8, g * 0.8, b * 0.8);
  }
  g_assert(COLOR_MARKER_END == COLOR_MARKER_START + 31);
  for (i = 0; i < 32; i++) {
    double h = 1.0 + i * (3.0 / 32.0);
    double r, g, b;
    if (h >= 3.0) h -= 3.0;
    hsv2rgb(game, h, 1.0, 1.0, &r, &g, &b);
    SET_COLOR(COLOR_MARKER_START + i, r, g, b);
  }
  g_assert(COLOR_RATE_END == COLOR_RATE_START + 31);
  for (i = 0; i < 32; i++) {
    double x = pow(0.2 * (1.0 - i / 31.0), game->opts->igamma);
    SET_COLOR(COLOR_RATE_START + i, x, x, x);
  }
  gdk_colormap_alloc_colors(gdk_colormap_get_system(), game->colors, NCOLOR, FALSE, TRUE, success);
}

/* If tv is non-NULL, use g_get_current_time to obtain it */
static double get_rt_from_time(VosGame *game, const GTimeVal *ptime)
{
  GTimeVal cur_time;

  if (game->opts->fast_mode) return game->start_rt;
  else {
    if (game->is_paused) cur_time = game->pause_time;
    else if (ptime != NULL) cur_time = *ptime;
    else g_get_current_time(&cur_time);
    return game->start_rt + timeval_diff(&game->start_time, &cur_time) * game->time_factor;
  }
}

static double get_rt(VosGame *game)
{
  return get_rt_from_time(game, NULL);
}

static void update_game_status(VosGame *game);
static void do_redraw(VosGame *game, double cur_rt);

static void do_refresh(VosGame *game, double cur_rt)
{
  GdkWindow *win = game->main_area->window;
  int width, height;
  GdkRectangle rect;

  if (win != NULL) {
    /*    g_message("do_refresh: %0.2f %0.2f", cur_rt * 75.0, get_rt(game) * 75.0); */
    gdk_drawable_get_size(GDK_DRAWABLE(win), &width, &height);
    rect.x = 0; rect.y = 0; rect.width = width; rect.height = height;
#if 0
    gdk_window_begin_paint_rect(win, &rect);
    do_redraw(game, cur_rt);
    gdk_window_end_paint(win);
#else
    gdk_window_invalidate_rect(win, &rect, FALSE);
#endif
  }
  if (cur_rt - game->last_status_upd_rt >= 0.25) update_game_status(game);
}

#define TRANSX(vx) ((int) floor(((double) (vx) - vx_min) * xstep + 0.5))
#define TRANSY(vy) ((int) floor(((double) (vy) - vy_min) * ystep + 0.5))

/* Note: GTK already provides double-buffering, so we need not do that */
static void do_redraw(VosGame *game, double cur_rt)
{
  VosSong *song = game->song;
  GdkWindow *win = game->main_area->window;
  GdkDrawable *d = GDK_DRAWABLE(win);
  GdkGC *gc, *gc2;
  int width, height;
  double xstep, ystep;
  unsigned i;
  int y0, y;

  if (win == NULL) return;
  gc = gdk_gc_new(d); gc2 = gdk_gc_new(d);
  gdk_drawable_get_size(d, &width, &height); width = MAX(width, 1); height = MAX(height, 1);
  xstep = (width - 1) / (vx_max - vx_min); ystep = (height - 1) / (vy_max - vy_min);
  y0 = TRANSY(0.0);
  /* Clear */
  gdk_gc_set_foreground(gc, &game->colors[COLOR_BLACK]);
  gdk_draw_rectangle(d, gc, TRUE, 0, 0, width, height);
  /* Keypresses */
  for (i = 0; i < NKEY; i++)
    if (game->keys[i].key_down) {
      int x0 = TRANSX(i), x1 = TRANSX(i + 1);
      int c = floor((cur_rt - game->keys[i].key_down_rt) * 20.0);
      unsigned color = COLOR_KEYPRESS_START + MIN(MAX(c, 0), COLOR_KEYPRESS - COLOR_KEYPRESS_START);
      gdk_gc_set_foreground(gc, &game->colors[color]);
      gdk_draw_rectangle(d, gc, TRUE, x0 + 1, 0, x1 - x0 - 1, height - 1);
    }
#if 0
  /* Rectangle showing the recent percentage */
  {
    double sx = 2.0, sy = 0.12; /* Half-size on the x and y axis */
    const GameScore *sc = &game->recent_scores[game->opts->recent_scores_nperiod-1], *sc0 = &game->recent_scores[0];
    double time = (game->opts->recent_scores_nperiod - 1) * game->opts->score_refresh_period; /* Actual time */
    double rate = (sc->score - sc0->score) / time, total_rate = (sc->total_score - sc0->total_score) / time;
    double x = (total_rate == 0.0) ? 1.0 : (rate / total_rate);
    unsigned idx;
    if (x < 0.0) x = 0.0; if (x > 1.0) x = 1.0;
    idx = (unsigned) floor(x * x * x * x * 32.0); if (idx > 31) idx = 31;
    gdk_gc_set_foreground(gc, &game->colors[COLOR_RATE_START + idx]);
    gdk_draw_rectangle(d, gc, TRUE, TRANSX(vx_mid - sx), TRANSY(-0.3 - sy),
		       (int) floor(2.0 * sx * xstep), (int) floor(2.0 * sy * ystep));
  }
#endif
  /* Vertical bars */
  gdk_gc_set_foreground(gc, &game->colors[COLOR_WHITE]);
  for (i = 0; i <= NKEY; i++) {
    int x = TRANSX(i);
    gdk_draw_line(d, gc, x, 0, x, height - 1);
  }
  /* Notes */
  for (i = 0; i < NKEY; i++) {
    int x0 = TRANSX(i + 0.1), x1 = TRANSX(i + 0.9);
    int x0r = TRANSX(i + 0.1 + note_vw), x1l = TRANSX(i + 0.9 - note_vw);
    double tmin = cur_rt - vy_max / game->speed_rt, tmax = cur_rt - vy_min / game->speed_rt;
    unsigned idx = game->cur_un_idxs[i];
    UserNote *un;

    if (idx == IDX_NONE) continue; /* No notes on that key */
    while (un = &song->user_notes[idx], un->rt_stop >= tmin && un->prev_idx != IDX_NONE) idx = un->prev_idx;
    while (un = &song->user_notes[idx], un->rt_stop < tmin && un->next_idx != IDX_NONE) idx = un->next_idx;
    while (un = &song->user_notes[idx], un->rt_start < tmax) {
      double t0 = MAX(un->rt_start, tmin), t1 = MIN(un->rt_stop, tmax);
      unsigned color = COLOR_NOTE_START + un->color * 2;
      if (un->is_long && t0 < t1) { /* Stem */
	double vy0 = (cur_rt - t0) * game->speed_rt, vy1 = (cur_rt - t1) * game->speed_rt;
	int y0 = TRANSY(vy0), y1 = TRANSY(vy1);
	if (y1 < y0) {
	  gdk_gc_set_foreground(gc, &game->colors[color]);
	  gdk_draw_rectangle(d, gc, TRUE, x0, y1, x0r - x0 + 1, y0 - y1);
	  gdk_draw_rectangle(d, gc, TRUE, x1l, y1, x1 - x1l + 1, y0 - y1);
	  gdk_draw_line(d, gc, x0, y1, x0, y0 - 1);
	  gdk_draw_line(d, gc, x1, y1, x1, y0 - 1);
	}
      }
      if (un->rt_start >= tmin && un->rt_start < tmax) { /* Lower cap */
	double vy_start = (cur_rt - un->rt_start) * game->speed_rt;
	int y_start = TRANSY(vy_start), y_start_top = TRANSY(vy_start - note_vt);
	gdk_gc_set_foreground(gc, &game->colors[color]);
	gdk_draw_rectangle(d, gc, TRUE, x0, y_start_top, x1 + 1 - x0, y_start - y_start_top);
      }
      if (un->is_long && un->rt_stop >= tmin && un->rt_stop < tmax) { /* Upper cap */
	double vy_stop = (cur_rt - un->rt_stop) * game->speed_rt;
	int y_stop = TRANSY(vy_stop), y_stop_top = TRANSY(vy_stop - note_vt);
	gdk_gc_set_foreground(gc, &game->colors[color]);
	gdk_draw_rectangle(d, gc, TRUE, x0, y_stop_top, x1 + 1 - x0, y_stop - y_stop_top);
      }
      if (un->next_idx != IDX_NONE) idx = un->next_idx;
      else break;
    }
    game->cur_un_idxs[i] = idx;
  }
  /* Markers */
  gdk_gc_set_line_attributes(gc2, 5, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);
  for (i = 0; i < NKEY; i++) {
    GQueue *markers = game->keys[i].markers;
    Marker *m;
    GList *p;
    int x0 = TRANSX(i + 0.5), y0 = TRANSY(0);

    while (m = (Marker *) g_queue_peek_head(markers), m != NULL && (cur_rt - m->rt) * game->tscale_rt > 1.0) {
      g_free(m); g_queue_pop_head(markers); /* Pop old markers */
    }
    p = markers->head;
    while (p) {
      int n, r;
      double norm_err, vr;
      
      m = (Marker *) p->data;
      /* NOTE: vr might be negative because of various reasons */
      vr = (cur_rt - m->rt) * game->tscale_rt * 0.4 + 0.4; vr = MAX(vr, 0.0); r = (int) floor(vr * xstep);
      g_assert(m->score <= 1.0); norm_err = sqrt(0.5 * (1.0 - m->score)); if (m->dt < 0.0) norm_err = -norm_err;
      n = (int) floor(norm_err * 16 + 0.5);
      if (n < -16) n = -16; if (n > 16) n = 16; if (n < 0) n += 32;
      gdk_gc_set_foreground(gc2, &game->colors[COLOR_MARKER_START+n]);
      gdk_draw_arc(d, gc2, FALSE, x0 - r, y0 - r, 2 * r, 2 * r, 0, 64 * 360);
      p = p->next;
    }
  }
  /* Rotating markers of long notes */
  for (i = 0; i < NKEY; i++) {
    KeyState *ks = &game->keys[i];
    if (ks->key_down && (! ks->long_okay || ks->long_scored)) {
      unsigned color = ks->long_scored ? COLOR_MARKER_BEST : COLOR_MARKER_WORST;
      double t = cur_rt / game->opts->init_time_factor;
      double angle = (t - floor(t)) * (2.0 * G_PI);
      int x0 = TRANSX(i + 0.5), y0 = TRANSY(0);
      int dx = ceil(0.4 * xstep * cos(angle)), dy = ceil(0.4 * xstep * sin(angle));
      gdk_gc_set_foreground(gc2, &game->colors[color]);
      gdk_draw_line(d, gc2, x0 - dx, y0 - dy, x0 + dx, y0 + dy);
      gdk_draw_line(d, gc2, x0 - dy, y0 + dx, x0 + dy, y0 - dx);
    }
  }
  /* Horizontal bars */
  gdk_gc_set_foreground(gc, &game->colors[COLOR_WHITE]);
  y = TRANSY(0.0); gdk_draw_line(d, gc, 0, y, width - 1, y); /* Lower line */
  y = TRANSY(-note_vt) - 1; gdk_draw_line(d, gc, 0, y, width - 1, y); /* Upper line */
  g_object_unref(G_OBJECT(gc)); g_object_unref(G_OBJECT(gc2));

  /* gdk_flush(); */
}

static gboolean on_main_area_expose(GtkWidget *widget, GdkEventExpose *ev, gpointer user_data)
{
  VosGame *game = (VosGame *) user_data;

  do_redraw(game, get_rt(game));
  return TRUE; /* End event handling */
}

static gboolean on_main_window_delete(GtkWidget *widget, GdkEvent *ev, gpointer user_data)
{
  gtk_main_quit();
  return TRUE; /* End event handling */
}

static void init_snd_seq_event(VosGame *game, snd_seq_event_t *sev)
{
  snd_seq_ev_clear(sev);
  snd_seq_ev_set_source(sev, game->seq_port_id);
  snd_seq_ev_set_dest(sev, game->dest_client_id, game->dest_port_id);
  snd_seq_ev_set_direct(sev);
}

static void set_master_volume(VosGame *game)
{
  double volume;
  unsigned ivol;
  snd_seq_event_t sev;
  unsigned char data[8] = {0xf0, 0x7f, 0x7f, 0x04, 0x01, 0x00, 0x00, 0xf7}; /* Data for the Master Volume SysEx */
  
  if (! game->opts->enable_sound) return;
  volume = (game->is_paused || game->is_resync) ? 0.0 : 1.0;
  g_assert(volume >= 0.0 && volume <= 1.0);
  if (game->volume == volume) return; game->volume = volume;
  ivol = (unsigned) floor(volume * 0x3fff); data[5] = ivol & 0x7f; data[6] = (ivol >> 7) & 0x7f;
  init_snd_seq_event(game, &sev);
  snd_seq_ev_set_sysex(&sev, sizeof(data), data);
  snd_seq_event_output(game->seq, &sev);
  snd_seq_drain_output(game->seq);
  /*  g_message("set master volume = %0.4f", volume); */
}

static void all_sounds_off(VosGame *game)
{
  unsigned chan;
  snd_seq_event_t sev;

  if (! game->opts->enable_sound) return;
  for (chan = 0; chan < 16; chan++) {
    init_snd_seq_event(game, &sev);
    snd_seq_ev_set_controller(&sev, chan, 120, 0);
    snd_seq_event_output(game->seq, &sev);
  }
  snd_seq_drain_output(game->seq);
}

static gboolean on_event_timeout(gpointer data);
static void process_key(VosGame *game, double cur_rt, unsigned key, gboolean is_keypress);
static void update_game_score(VosGame *game, double cur_rt);
static void get_game_score(VosGame *game, GameScore *out);
static void refresh_scores(VosGame *game, double cur_rt);
static void set_time_factor(VosGame *game, double time_factor, gboolean from_process_events);
/* Process the events in the scheduler whose time is up, and set the timeout for the next one */
static void process_events(VosGame *game)
{
  SchedEvent *ev;
  double cur_rt;
  gboolean found_events;
  gboolean is_quit = FALSE;
  gboolean did_output = FALSE;

 restart:
  cur_rt = get_rt(game);
  found_events = FALSE;
  while ((ev = get_first_event(game->sched)) != NULL) {
    if (cur_rt >= ev->rt) {
      found_events = TRUE;
      /* g_message("Event actual_rt=%0.3f cur_rt=%0.3f rt=%0.3f type=%d src=%u idx=%u",
	 get_rt(game), cur_rt, ev->rt, ev->type, ev->src, ev->idx); */
      if (ev->type == SchedEventMidi || ev->type == SchedEventNoteOn
	  || ev->type == SchedEventNoteOff || ev->type == SchedEventUserNoteOff) { /* May need to do MIDI output */
	
	snd_seq_event_t sev;
	gboolean no_output = FALSE;

	init_snd_seq_event(game, &sev);
	if (ev->type == SchedEventMidi) {
	  MidiTrack *track = &game->song->tracks[ev->src];
	  MidiEvent *mev = &track->events[ev->idx];
	  unsigned cmd_type = (mev->cmd & 0x70) >> 4, chan = mev->cmd & 0x0f;
	  int pitch;
	  switch (cmd_type) {
	  case 0: snd_seq_ev_set_noteoff(&sev, chan, mev->a, mev->b); break; /* Note off */
	  case 1: snd_seq_ev_set_noteon(&sev, chan, mev->a, mev->b); break; /* Note on */
	  case 2: snd_seq_ev_set_keypress(&sev, chan, mev->a, mev->b); break; /* Key pressure */
	  case 3: snd_seq_ev_set_controller(&sev, chan, mev->a, mev->b); break; /* Controller */
	  case 4: snd_seq_ev_set_pgmchange(&sev, chan, mev->a); break; /* Program change */
	  case 5: snd_seq_ev_set_chanpress(&sev, chan, mev->a); break; /* Channel pressure */
	  case 6: /* Pitch wheel */
	    pitch = ((int) (((unsigned) (mev->b & 0x7f) << 7) | (unsigned) (mev->a & 0x7f))) - 0x2000;
	    snd_seq_ev_set_pitchbend(&sev, chan, pitch);
	    break;
	  case 7: g_assert_not_reached(); break;
	  }
#if 0
	  if (cmd_type == 3 && mev->a == 64) no_output = TRUE; /* Hold Pedal doesn't sound good (but see the -m option of timidity) */
#endif
	  if (ev->idx + 1 < track->nevent) {
	    MidiEvent *mev = &track->events[ev->idx+1];
	    double rt = tempoit_tt_to_rt(game->song, &game->it, mev->tt);
	    sched_event(game->sched, rt, SchedEventMidi, ev->src, ev->idx + 1);
	  }
	} else if (ev->type == SchedEventNoteOn || ev->type == SchedEventNoteOff) {
	  NoteArray *arr = &game->song->note_arrays[ev->src];
	  Note *note = &arr->notes[ev->idx];
	  unsigned chan = (note->cmd & 0x0f);
	  unsigned char vol = game->opts->half_volume_for_bgm ? (note->vol + 1) / 2 : note->vol; /* Change the volume for BGM */

	  if (note->is_user) no_output = TRUE;
	  if (ev->type == SchedEventNoteOn) {
	    double rt_end = tempoit_tt_to_rt(game->song, &game->it, note->tt_end);

	    snd_seq_ev_set_noteon(&sev, chan, note->note_num, vol);
	    sched_event(game->sched, rt_end, SchedEventNoteOff, ev->src, ev->idx);
	    if (ev->idx + 1 < arr->num_note) {
	      Note *note = &arr->notes[ev->idx+1];
	      double rt = tempoit_tt_to_rt(game->song, &game->it, note->tt_start);
	      sched_event(game->sched, rt, SchedEventNoteOn, ev->src, ev->idx + 1);
	    }
	  } else snd_seq_ev_set_noteoff(&sev, chan, note->note_num, vol);
	} else if (ev->type == SchedEventUserNoteOff) { /* Delayed user note off */
	  UserNote *un = &game->song->user_notes[ev->idx];
	  snd_seq_ev_set_noteoff(&sev, un->cmd & 0x0f, un->note_num, un->vol);
	} else g_assert_not_reached();
	if (! no_output && game->opts->enable_sound) { snd_seq_event_output(game->seq, &sev); did_output = TRUE; }
	
      } else { /* Non-MIDI events */
	if (ev->type == SchedEventRefreshScreen) {
	  double next_rt;
	  g_assert(game->opts->enable_ui);
	  /* next_rt = ev->rt; while (next_rt <= cur_rt) next_rt += game->frame_time; */
	  next_rt = cur_rt + game->frame_time * game->time_factor;
	  refresh_scores(game, ev->rt); /* For rotating markers of long notes */
	  do_refresh(game, cur_rt); /* use cur_rt to reduce lag as much as possible */
	  if (game->opts->adaptive_time_factor) { /* Adjust the speed to a comfortable level */
	    const double adj_rate = 0.2; /* fraction per second */
	    double speed = game->speed_rt * game->time_factor, speed0 = game->opts->init_speed, new_speed;
	    double max_adj = exp(adj_rate * (next_rt - ev->rt) / game->time_factor);
	    new_speed = (speed < speed0) ? MIN(speed0, speed * max_adj) : MAX(speed0, speed / max_adj);
	    game->speed_rt = new_speed / game->time_factor;
	  }
	  sched_event(game->sched, next_rt, SchedEventRefreshScreen, 0, 0);
	} else if (ev->type == SchedEventDemo) {
	  const InputEvent *iev = &game->in_evs[ev->idx];
	  process_key(game, iev->rt, iev->key, iev->is_keypress);
	  if (ev->idx + 1 < game->in_nev) {
	    const InputEvent *iev = &game->in_evs[ev->idx+1];
	    sched_event(game->sched, iev->rt, SchedEventDemo, 0, ev->idx + 1);
	  }
	} else if (ev->type == SchedEventRefreshScore) {
	  unsigned nperiod = game->opts->recent_scores_nperiod;
	  GameScore sc, last_sc, last_sc1; /* last_sc1 does no averaging */
	  char *text;
	  unsigned i;
	  double time = nperiod * game->opts->score_refresh_period; /* Actual time */
	  double rate, total_rate, ratio;

	  update_game_score(game, ev->rt); /* Which calls refresh_scores() */
	  get_game_score(game, &sc);
	  last_sc = game->recent_scores[0]; last_sc1 = game->recent_scores[nperiod-1];
	  for (i = 1; i < nperiod; i++) game->recent_scores[i-1] = game->recent_scores[i];
	  game->recent_scores[nperiod-1] = sc;
	  rate = (sc.score - last_sc.score) / time; total_rate = (sc.total_score - last_sc.total_score) / time;
	  ratio = (total_rate == 0.0) ? 1.0 : (rate / total_rate);
	  text = g_strdup_printf("Rate: %0.2f / %0.2f (%0.2f%%)", rate, total_rate, ratio * 100.0);
	  gtk_label_set_text(game->rate_label, text);
	  g_free(text);
	  if (game->opts->adaptive_time_factor && ! game->is_resync) {
	    const double adj_rate = 0.2; /* Per second of actual time */
	    /* We use the instantaneous rate below */
	    double time = game->opts->score_refresh_period;
	    double rate = (sc.score - last_sc1.score) / time, total_rate = (sc.total_score - last_sc1.total_score) / time;
	    if (total_rate > 0.0) {
	      const double rate0 = 20.0;
	      double weight = MIN(total_rate, rate0) / rate0;
	      double ratio = rate / total_rate;
	      double deviation, new_time_factor;
	      deviation = ratio - game->opts->target_ratio;
	      deviation = MIN(deviation, 1.0); deviation = MAX(deviation, -1.0);
	      new_time_factor = game->time_factor * exp(deviation * weight * adj_rate * game->opts->score_refresh_period);
	      new_time_factor = MIN(new_time_factor, 2.0); new_time_factor = MAX(new_time_factor, 0.2);
	      set_time_factor(game, new_time_factor, TRUE);
	    }
	  }
	  if (ev->rt >= game->song->max_rt + 1.0) is_quit = TRUE;
	  else sched_event(game->sched, ev->rt + game->opts->score_refresh_period * game->time_factor,
			   SchedEventRefreshScore, 0, 0);
	} else if (ev->type == SchedEventAutoOn || ev->type == SchedEventAutoOff) {
	  UserNote *un = &game->song->user_notes[ev->idx];
	  double rt_end = un->rt_end;
	  if (un->next_idx != IDX_NONE) rt_end = MIN(rt_end, game->song->user_notes[un->next_idx].rt_start);
	  if (! un->is_long) rt_end = MIN(rt_end, un->rt_start + 0.1);
	  if (ev->type == SchedEventAutoOn) {
	    process_key(game, un->rt_start, un->key, TRUE);
	    sched_event(game->sched, rt_end, SchedEventAutoOff, ev->src, ev->idx);
	  } else {
	    process_key(game, rt_end, un->key, FALSE);
	    if (un->next_idx != IDX_NONE)
	      sched_event(game->sched, game->song->user_notes[un->next_idx].rt_start, SchedEventAutoOn,
			  ev->src, un->next_idx);
	  }
	} else g_assert_not_reached();
      }
      remove_event(game->sched, ev);
    } else break;
  }
  if (did_output) snd_seq_drain_output(game->seq);
  if (found_events) goto restart; /* Maybe time has changed... */
  if (game->is_resync) all_sounds_off(game);
  game->is_resync = FALSE; set_master_volume(game);
  if (! game->opts->fast_mode) {
    if (ev == NULL) game->event_timeout_id = 0;
    else {
      int dt = (int) ceil((ev->rt - cur_rt) / game->time_factor * 1000.0);
      g_assert(dt > 0);
      /*      g_message("Schedule: dt=%d ms", dt); */
      game->event_timeout_id = gtk_timeout_add(dt, on_event_timeout, game);
    }
    if (is_quit) gtk_main_quit();
  } else {
    if (is_quit || ev == NULL) gtk_main_quit();
    else { game->start_rt = ev->rt; goto restart; }
  }
}

static gboolean on_event_timeout(gpointer data)
{
  VosGame *game = (VosGame *) data;

  process_events(game);
  return FALSE; /* remove the timeout */
}

static void add_marker(VosGame *game, unsigned key, double rt, double dt, double score)
{
  Marker *m;

  m = g_new0(Marker, 1); m->rt = rt; m->dt = dt; m->score = score;
  g_queue_push_tail(game->keys[key].markers, m);
}

/* Update the state of the comparer after an event at cur_rt.
   If is_dummy is true, just update the tables with the new cur_rt, but there is no real event.
   If pdt is non-NULL, it is filled with the best-matching dt, or 0.0 if the best match is to ignore it. */
#define PREV_IDX(un) ((is_keypress) ? (un)->prev_idx : (un)->prev_idx_long)
#define NEXT_IDX(un) ((is_keypress) ? (un)->next_idx : (un)->next_idx_long)
#define RT(un) ((is_keypress) ? (un)->rt_start : (un)->rt_end)
#define COUNT(un) ((is_keypress) ? (un)->count : (un)->count_long)
static void comparer_add_event(VosGame *game, double cur_rt, unsigned key, gboolean is_keypress,
			       gboolean is_dummy, double *pdt)
{
  KeyState *ks = &game->keys[key];
  Comparer *cmp = (is_keypress) ? &ks->cmp_press : &ks->cmp_release;
  double score_dt0_rt = game->opts->score_dt0 * game->time_factor;
  double min_score = (is_keypress) ? -1.0 : 0.0, max_dt = sqrt((1.0 - min_score) * 0.5) * score_dt0_rt;
  double min_rt = cur_rt - max_dt, max_rt = cur_rt + max_dt;
  unsigned idx = (is_keypress) ? game->cur_un_idxs[key] : game->cur_un_idxs_long[key];
  unsigned prev_idx, next_idx;
  UserNote *un;
  double un_rt;
  ComparerEntry ent;
  unsigned i0, i1;
  GArray *new_ents_arr;
  double score0, last_score, score_i0, score_i1;
  unsigned count0;

  if (pdt) *pdt = 0.0;
  if (idx == IDX_NONE) { /* No (long) note on this key */
    g_assert(cmp->nent == 0); if (! is_dummy) cmp->score0 += min_score;
  } else {
    new_ents_arr = g_array_new(FALSE, FALSE, sizeof(ComparerEntry));
    while (un = &game->song->user_notes[idx], prev_idx = PREV_IDX(un),
	   prev_idx != IDX_NONE && RT(&game->song->user_notes[prev_idx]) >= min_rt)
      idx = prev_idx;
    while (un = &game->song->user_notes[idx], next_idx = NEXT_IDX(un),
	   RT(un) < min_rt && next_idx != IDX_NONE)
      idx = next_idx;
    count0 = COUNT(un) - 1; if (RT(un) < min_rt) count0++;
    i0 = i1 = 0; score_i0 = score_i1 = cmp->score0;
    while (i1 < cmp->nent && RT(&game->song->user_notes[cmp->ents[i1].last_idx]) < min_rt) {
      score_i1 = cmp->ents[i1].score; i1++;
    }
    score0 = (is_dummy) ? score_i1 : score_i1 + min_score; last_score = score0;
    while (un = &game->song->user_notes[idx], un_rt = RT(un),
	   un_rt >= min_rt && un_rt <= max_rt) {
      double dt = un_rt - cur_rt, norm_err = dt / score_dt0_rt;
      double note_score = 1.0 - 2.0 * norm_err * norm_err;
      double score_match, score_ignore;
      note_score = MAX(note_score, min_score);
      while (i0 < cmp->nent && cmp->ents[i0].last_idx < idx) { score_i0 = cmp->ents[i0].score; i0++; }
      while (i1 < cmp->nent && cmp->ents[i1].last_idx <= idx) { score_i1 = cmp->ents[i1].score; i1++; }
      score_match = (is_dummy) ? score_i0 : score_i0 + note_score; /* Match the keypress with un */
      score_ignore = (is_dummy) ? score_i1 : score_i1 + min_score; /* Ignore the keypress */
      ent.last_idx = idx; ent.score = MAX(score_match, score_ignore);
#if 0
      if (! is_dummy)
	g_message("%s un_rt=%0.3f, cur_rt=%0.3f, note_score=%0.3f last_score=%0.3f ent_score=%0.3f",
		  (is_keypress) ? "PRESS" : "RELEASE",
		  un_rt, cur_rt, note_score, last_score, ent.score);
#endif
      if (ent.score > last_score) {
	last_score = ent.score; g_array_append_val(new_ents_arr, ent);
	if (pdt) *pdt = (score_match == ent.score) ? dt : 0.0;
      }
      next_idx = NEXT_IDX(un); if (next_idx != IDX_NONE) idx = next_idx; else break;
    }
    if (is_keypress) game->cur_un_idxs[key] = idx; else game->cur_un_idxs_long[key] = idx;
    g_free(cmp->ents); cmp->count0 = count0; cmp->score0 = score0; cmp->nent = new_ents_arr->len;
    cmp->ents = (ComparerEntry *) g_array_free(new_ents_arr, FALSE);
  }
}
#undef PREV_IDX
#undef NEXT_IDX
#undef RT
#undef COUNT

static void comparer_get_score_count(VosGame *game, unsigned key, gboolean is_keypress, double *pscore, unsigned *pcount)
{
  const KeyState *ks = &game->keys[key];
  const Comparer *cmp = (is_keypress) ? &ks->cmp_press : &ks->cmp_release;
  
  if (cmp->nent == 0) { if (pscore) *pscore = cmp->score0; if (pcount) *pcount = cmp->count0; }
  else {
    const ComparerEntry *ent = &cmp->ents[cmp->nent-1];
    const UserNote *un = &game->song->user_notes[ent->last_idx];
    if (pscore) *pscore = ent->score;
    if (pcount) *pcount = (is_keypress) ? un->count : un->count_long;
  }
}

/* Count the long_score in the duration between long_rt and cur_rt, and update long_rt to cur_rt */
static void calc_long_score(VosGame *game, double cur_rt, unsigned key, gboolean key_down)
{
  UserNote *un;
  unsigned idx = game->cur_un_idxs[key], prev_idx;
  KeyState *ks = &game->keys[key];
  double rt0, rt1;
  double cur_time, okay_time = 0.0, scored_time = 0.0, total_time = cur_rt - ks->long_rt;
  double allowed_hold_time_rt = game->opts->allowed_hold_time * game->time_factor;
  gboolean is_okay = FALSE, is_scored = FALSE; /* Status at cur_rt */

  if (total_time < 0.0) return; /* May happen if the user pressed some key during the wait time, or
				   due to small timing errors. */
  if (idx == IDX_NONE) { ks->long_rt = cur_rt; return; }
  while (un = &game->song->user_notes[idx], prev_idx = un->prev_idx,
	 prev_idx != IDX_NONE && game->song->user_notes[prev_idx].rt_end + allowed_hold_time_rt >= ks->long_rt)
    idx = prev_idx;
  /* NOTE: we do not go forward here, since nonoverlapping notes do not affect the calculation below */
  while (un = &game->song->user_notes[idx],
	 un->rt_start - allowed_hold_time_rt <= cur_rt) {
    /* Calculate okay_time */
    rt0 = un->rt_start - allowed_hold_time_rt; rt1 = un->rt_end + allowed_hold_time_rt;
    /* If the allowed_hold_time of two adjacent user notes overlap, it is counted towards the second note.
       We must be careful here, otherwise we would overestimate okay_time. */
    if (un->next_idx != IDX_NONE)
      rt1 = MIN(rt1, game->song->user_notes[un->next_idx].rt_start - allowed_hold_time_rt);
    if (rt0 > cur_rt) break;
    cur_time = MIN(rt1, cur_rt) - MAX(rt0, ks->long_rt); cur_time = MAX(cur_time, 0.0);
    okay_time += cur_time; if (rt1 >= cur_rt) is_okay = TRUE;
    if (un->is_long) { /* Calculate scored_time */
      rt0 = un->rt_start; rt1 = un->rt_end;
      if (un->next_idx != IDX_NONE) rt1 = MIN(rt1, game->song->user_notes[un->next_idx].rt_start);
      if (rt0 > cur_rt) break;
      cur_time = MIN(rt1, cur_rt) - MAX(rt0, ks->long_rt); cur_time = MAX(cur_time, 0.0);
      scored_time += cur_time; if (rt1 >= cur_rt) is_scored = TRUE;
    }
    if (un->next_idx == IDX_NONE) break; else idx = un->next_idx;
  }
  g_assert(okay_time >= 0.0); g_assert(okay_time <= total_time + 1e-6);
  if (key_down) ks->long_score += scored_time - (total_time - okay_time);
  ks->total_long_score += scored_time; ks->long_rt = cur_rt; ks->long_okay = is_okay; ks->long_scored = is_scored;
  game->cur_un_idxs[key] = idx;
}

static void refresh_scores(VosGame *game, double cur_rt)
{
  unsigned key;

  for (key = 0; key < NKEY; key++) {
    KeyState *ks = &game->keys[key];
    comparer_add_event(game, cur_rt, key, TRUE, TRUE, NULL); comparer_add_event(game, cur_rt, key, FALSE, TRUE, NULL);
    calc_long_score(game, cur_rt, key, ks->key_down);
  }
}

/* cur_rt does not have to be the current time, but it must be monotonic */
static void process_key(VosGame *game, double cur_rt, unsigned key, gboolean is_keypress)
{
  KeyState *ks = &game->keys[key];
  snd_seq_event_t sev;
  double orig_score, new_score, delta_score, score_dt;

  /* NOTE: cur_rt is not quantized which may cause some minor errors in demo recording.  However,
     quantizing cur_rt may cause the scoring process to lose time-ordering, which can be very
     troublesome. */
  if (is_keypress && ks->key_down) return; /* Probably due to auto-repeat */
  if (! is_keypress && ! ks->key_down) { g_warning("Unexpected release of key %u", key); return; }
  ks->key_down = is_keypress; if (is_keypress) ks->key_down_rt = cur_rt;

  if (game->opts->mode == VosGameModeNormal) { /* Record the keypress */
    InputEvent ev;
    ev.rt = cur_rt; ev.is_keypress = is_keypress; ev.key = key;
    g_array_append_val(game->input_events_arr, ev);
  }

  comparer_get_score_count(game, key, is_keypress, &orig_score, NULL);
  comparer_add_event(game, cur_rt, key, is_keypress, FALSE, &score_dt);
  comparer_get_score_count(game, key, is_keypress, &new_score, NULL);
  delta_score = new_score - orig_score; /* Score gain of this event */
  calc_long_score(game, cur_rt, key, is_keypress ? FALSE : TRUE);
  update_game_score(game, cur_rt);

  init_snd_seq_event(game, &sev);
  if (is_keypress) { /* Need to find the closest UserNote to play; dt should be of the correct sign most of the time */
    unsigned idx = game->cur_un_idxs[key], best_idx;
    double dt, best_dt;
    UserNote *un;

    if (idx == IDX_NONE) ks->un_idx = IDX_NONE;
    else {
      while (un = &game->song->user_notes[idx], un->rt_start >= cur_rt && un->prev_idx != IDX_NONE)
	idx = un->prev_idx; /* Go back to some note before the cur_rt, or the first note */
      best_idx = IDX_NONE; best_dt = 100.0;
      while (1) {
	dt = un->rt_start - cur_rt;
	/* Key pressed in the middle of a long note should be regarded as an attempt to finish it */
	if (un->is_long && dt <= 0.0 && un->rt_end - cur_rt >= 0.0) dt = 0.0;
	if (best_idx == IDX_NONE || fabs(dt) < fabs(best_dt)) { best_idx = idx; best_dt = dt; }
	if (un->rt_start >= cur_rt) break;
	if (un->next_idx == IDX_NONE) break;
	idx = un->next_idx; un = &game->song->user_notes[idx];
      }
      game->cur_un_idxs[key] = idx;
      ks->un_idx = idx = best_idx; un = &game->song->user_notes[idx];
      snd_seq_ev_set_noteon(&sev, un->cmd & 0x0f, un->note_num, un->vol);
      if (game->opts->enable_sound) snd_seq_event_output(game->seq, &sev);
      snd_seq_drain_output(game->seq);
    }
  } else {
    if (ks->un_idx != IDX_NONE) {
      unsigned idx = ks->un_idx;
      UserNote *un = &game->song->user_notes[idx];
      ks->un_idx = IDX_NONE;
      /* Automatically lengthen short notes if needed */
      if (short_note_auto_len && ! un->is_long && cur_rt < un->rt_end)
	sched_event(game->sched, un->rt_end, SchedEventUserNoteOff, 0, idx);
      else {
	snd_seq_ev_set_noteoff(&sev, un->cmd & 0x0f, un->note_num, un->vol);
	if (game->opts->enable_sound) snd_seq_event_output(game->seq, &sev);
	snd_seq_drain_output(game->seq);
      }
    }
  }
  /*  g_message("%s dt=%0.4f delta_score=%0.4f", is_keypress ? " PRESS " : "RELEASE", score_dt, delta_score); */
  if (is_keypress || delta_score > 0.0) add_marker(game, key, cur_rt, score_dt, delta_score);
}

static void update_game_state(VosGame *game)
{
  gtk_label_set_text(game->state_label, (game->is_paused) ? "Paused" : "Running");
}

/* Things like time etc. */
static void update_game_status(VosGame *game)
{
  char *text;
  double cur_rt = get_rt(game);
  unsigned cur_rt_nsec = (unsigned) floor(MAX(cur_rt, 0.0));
  unsigned total_nsec = (unsigned) floor(game->song->max_rt);

  game->last_status_upd_rt = cur_rt;
  text = g_strdup_printf("Time: %u:%02u / %u:%02u", cur_rt_nsec / 60, cur_rt_nsec % 60,
			 total_nsec / 60, total_nsec % 60);
  gtk_label_set_text(game->time_label, text);
  g_free(text);
}

static void get_game_score(VosGame *game, GameScore *out)
{
  double score = 0.0, total_score = 0.0;
  unsigned key;

  for (key = 0; key < NKEY; key++) {
    const KeyState *ks = &game->keys[key];
    double cur_score;
    unsigned cur_count;
    
    comparer_get_score_count(game, key, TRUE, &cur_score, &cur_count);
    score += cur_score; total_score += cur_count;
    comparer_get_score_count(game, key, FALSE, &cur_score, &cur_count);
    score += cur_score; total_score += cur_count;
    score += ks->long_score; total_score += ks->total_long_score;
  }
  out->score = score; out->total_score = total_score;
}

static void update_game_score(VosGame *game, double cur_rt)
{
  GameScore sc;
  char *text;

  refresh_scores(game, cur_rt);
  get_game_score(game, &sc);
  text = g_strdup_printf("Score: %0.2f / %0.2f (%0.2f%%)", sc.score, sc.total_score,
			 (sc.total_score == 0.0) ? 100.0 : (sc.score / sc.total_score * 100.0));
  gtk_label_set_text(game->score_label, text);
  g_free(text);
}

/* To avoid messing up game->event_timeout_id, process_events() must only be called from this timeout */
static void trigger_process_events(VosGame *game)
{
  if (game->event_timeout_id) { gtk_timeout_remove(game->event_timeout_id); game->event_timeout_id = 0; }
  game->event_timeout_id = gtk_timeout_add(0, on_event_timeout, game);
}

static void pause_game(VosGame *game)
{
  double cur_rt;
  
  g_assert(! game->is_paused);
  game->is_paused = TRUE; set_master_volume(game);
  g_get_current_time(&game->pause_time); cur_rt = get_rt_from_time(game, &game->pause_time);
  if (game->event_timeout_id) { gtk_timeout_remove(game->event_timeout_id); game->event_timeout_id = 0; }
  refresh_scores(game, cur_rt); do_refresh(game, cur_rt);
  update_game_state(game); update_game_status(game);
}

static void unpause_game(VosGame *game)
{
  double cur_rt = get_rt_from_time(game, &game->pause_time);
  
  g_assert(game->is_paused);
  game->start_rt = cur_rt;
  game->is_paused = FALSE; set_master_volume(game);
  g_get_current_time(&game->start_time);
  trigger_process_events(game);
  refresh_scores(game, cur_rt); do_refresh(game, cur_rt);
  update_game_state(game); update_game_status(game);
}

static void update_time_factor_label(VosGame *game)
{
  char *text;

  text = g_strdup_printf("Time factor: %0.4f", game->time_factor);
  gtk_label_set_text(game->time_factor_label, text);
  g_free(text);
}

static void set_time_factor(VosGame *game, double time_factor, gboolean from_process_events)
{
  GTimeVal cur_time;
  double cur_rt;

  /*  g_message("set_time_factor(%0.4f)", time_factor); */
  g_assert(! game->is_paused);
  g_get_current_time(&cur_time); cur_rt = get_rt_from_time(game, &cur_time);
  game->start_time = cur_time; game->start_rt = cur_rt; game->time_factor = time_factor;
  update_time_factor_label(game);
  if (! from_process_events) trigger_process_events(game); /* The original timeout value might be too large */
}

static gboolean on_main_area_key(GtkWidget *widget, GdkEventKey *ev, gpointer user_data)
{
  VosGame *game = (VosGame *) user_data;
  double cur_rt = get_rt(game);
  unsigned key = IDX_NONE;
  gboolean is_keypress = (ev->type == GDK_KEY_PRESS);
  gboolean is_interactive = (game->opts->mode == VosGameModeNormal && ! game->is_paused);

  switch (ev->keyval) {
  case GDK_s: key = 0; break;
  case GDK_d: key = 1; break;
  case GDK_f: key = 2; break;
  case GDK_space: key = 3; break;
  case GDK_j: key = 4; break;
  case GDK_k: key = 5; break;
  case GDK_l: key = 6; break;
  case GDK_p:
    if (is_keypress) {
      if (! game->is_paused) pause_game(game);
      else unpause_game(game);
    }
    break;
  case GDK_Escape: gtk_main_quit(); break;
  case GDK_r: all_sounds_off(game); break;
  case GDK_g: if (is_interactive && game->opts->allow_cheating) set_time_factor(game, game->time_factor / 1.05, FALSE); break;
  case GDK_h: if (is_interactive && game->opts->allow_cheating) set_time_factor(game, game->time_factor * 1.05, FALSE); break;
  default: return FALSE;
  }
  if (key != IDX_NONE && is_interactive) process_key(game, cur_rt, key, is_keypress);
  return TRUE; /* event handled */
}

static guint32 pev_read_u32(FILE *file)
{
  guint32 tmp;
  int result;
  result = fread(&tmp, 4, 1, file); g_assert(result == 1);
  return GUINT32_FROM_LE(tmp);
}

static void pev_write_u32(FILE *file, guint32 value)
{
  guint32 tmp = GUINT32_TO_LE(value);
  int result;
  result = fwrite(&tmp, 4, 1, file); g_assert(result == 1);
}

static double pev_read_double(FILE *file)
{
  union { guint64 u64; double dbl; } u;
  guint64 tmp;
  int result;
  
  result = fread(&tmp, 8, 1, file); g_assert(result == 1);
  u.u64 = GUINT64_FROM_LE(tmp);
  return u.dbl;
}

static void pev_write_double(FILE *file, double value)
{
  union { guint64 u64; double dbl; } u;
  guint64 tmp;
  int result;
  
  u.dbl = value;
  tmp = GUINT64_TO_LE(u.u64);
  result = fwrite(&tmp, 8, 1, file); g_assert(result == 1);
}

static unsigned pev_read_vl(FILE *file)
{
  unsigned value = 0, nshift = 0;
  int result;
  unsigned char ch;
  while (1) {
    result = getc(file); g_assert(result != EOF);
    ch = (unsigned) result;
    value |= (ch & 0x7f) << nshift; nshift += 7;
    if (! (ch & 0x80)) break;
  }
  return value;
}

static void pev_write_vl(FILE *file, unsigned value)
{
  unsigned char ch;
  int result;
  do {
    ch = value & 0x7f; value >>= 7;
    if (value != 0) ch |= 0x80;
    result = putc(ch, file); g_assert(result != EOF);
  } while (value != 0);
}

/* NOTE: the length field is already read */
static char *pev_read_str(FILE *file, unsigned len)
{
  char *str;
  int result;

  str = g_new0(char, len + 1);
  result = fread(str, 1, len, file); g_assert(result == (int) len);
  return str;
}

/* This also writes the length field.  NOTE: the written string is not NULL-terminated */
static void pev_write_str(FILE *file, const char *str)
{
  unsigned len = strlen(str);
  int result;
  pev_write_vl(file, len);
  result = fwrite(str, 1, len, file); g_assert(result == (int) len);
}

static void load_input_events(const char *in_fname, GArray *events_arr, unsigned char *hash,
			      gboolean *has_hash, GameOptions *opts)
{
  FILE *file;
  unsigned magic, type, len;
  time_t time;
  char *user_name;
  int irt, init_irt = 0;
  int result;

  *has_hash = FALSE;
  file = fopen(in_fname, "rb"); g_assert(file != NULL);
  magic = pev_read_u32(file); g_assert(magic == PEV_MAGIC);
  opts->tempo_factor = 1.0;
  while (1) {
    type = pev_read_vl(file); if (type == PEV_HEADER_END) break;
    len = pev_read_vl(file);
    switch (type) {
    case PEV_HASH_SHA1:
      g_assert(len == SHA_DIGEST_LENGTH);
      *has_hash = TRUE;
      result = fread(hash, SHA_DIGEST_LENGTH, 1, file); g_assert(result == 1);
      break;
    case PEV_TEMPO_FACTOR:
      g_assert(len == 8); opts->tempo_factor = pev_read_double(file);
      if (opts->tempo_factor != 1.0) g_message("Tempo factor: %0.4f", opts->tempo_factor);
      break;
      /* NOTE: Manual or adaptive changes to the time factor are not recorded; just look at cheat mode */
    case PEV_TIME_FACTOR:
      g_assert(len == 8); opts->init_time_factor = pev_read_double(file);
      if (opts->init_time_factor != 1.0) g_message("Initial time factor: %0.4f", opts->init_time_factor);
      break;
    case PEV_ADAPTIVE_TIME_FACTOR:
      opts->adaptive_time_factor = TRUE;
      if (len == 8) opts->target_ratio = pev_read_double(file); else g_assert(len == 0);
      g_message("Adaptive time factor mode is on (target_ratio = %0.4f)", opts->target_ratio);
      break;
    case PEV_CHEAT: g_assert(len == 0); opts->allow_cheating = TRUE; g_message("Cheat mode is on"); break;
    case PEV_SEEK: g_assert(len == 8); opts->seek = pev_read_double(file); opts->use_seek = TRUE; break;
    case PEV_SPEED: g_assert(len == 8); opts->init_speed = pev_read_double(file); break;
    case PEV_TIME_UNIT: g_assert(len == 8); opts->pev_time_unit = pev_read_double(file); break;
    case PEV_DATE:
      g_assert(len == 4); time = pev_read_u32(file);
      g_message("Demo recorded at %s", ctime(&time));
      break;
    case PEV_USER_NAME:
      user_name = pev_read_str(file, len);
      g_message("Recorded by: %s", user_name);
      g_free(user_name);
      break;
    case PEV_TIME_OFFSET: g_assert(len == 4); init_irt = -(int) pev_read_u32(file); break;
    default:
      g_warning("Unknown PEV header type %u", type);
      result = fseek(file, len, SEEK_CUR); g_assert(result == 0);
      break;
    }
  }
  irt = init_irt;
  while (1) {
    int idt;
    InputEvent ev;
    
    type = pev_read_vl(file);
    if (type == PEV_EV_END) break;
    else if (type >= PEV_EV_PRESS && type < PEV_EV_PRESS + NKEY) {
      ev.key = type - PEV_EV_PRESS; ev.is_keypress = TRUE;
    } else if (type >= PEV_EV_RELEASE && type < PEV_EV_RELEASE + NKEY) {
      ev.key = type - PEV_EV_RELEASE; ev.is_keypress = FALSE;
    } else g_assert_not_reached();
    idt = pev_read_vl(file); g_assert(idt >= 0); irt += idt; ev.rt = irt * opts->pev_time_unit;
    g_array_append_val(events_arr, ev);
    /*    g_message("event: pos=%u time=%0.3f", (unsigned) ftell(file), ev.rt); */
  }
  fclose(file);
}

static void save_input_events(VosGame *game, const char *out_fname)
{
  FILE *file;
  unsigned i, nev = game->input_events_arr->len;
  InputEvent *evs = (InputEvent *) g_array_free(game->input_events_arr, FALSE);
  int last_irt, init_irt;
  int result;

  if (game->opts->mode != VosGameModeNormal) goto no_save;
  if (out_fname != NULL) file = fopen(out_fname, "wb");
  else {
    char *fname;
    GError *err = NULL;
    int fd = g_file_open_tmp("pmg-save-XXXXXX", &fname, &err);
    if (fd == -1) {
      g_warning("Cannot write autosave'd event list file: %s.", err->message);
      g_error_free(err);
      goto no_save;
    }
    g_message("Autosave file name: %s", fname);
    g_free(fname);
    file = fdopen(fd, "wb");
  }
  if (file == NULL) { g_warning("Cannot write event list file %s.", out_fname); goto no_save; }
  /* Write file header */
  pev_write_u32(file, PEV_MAGIC);
  pev_write_vl(file, PEV_HASH_SHA1); pev_write_vl(file, SHA_DIGEST_LENGTH);
  result = fwrite(game->song->hash, SHA_DIGEST_LENGTH, 1, file); g_assert(result == 1);
  if (game->opts->tempo_factor != 1.0) {
    pev_write_vl(file, PEV_TEMPO_FACTOR); pev_write_vl(file, 8); pev_write_double(file, game->opts->tempo_factor);
  }
  if (game->opts->init_time_factor != 1.0) {
    pev_write_vl(file, PEV_TIME_FACTOR); pev_write_vl(file, 8); pev_write_double(file, game->opts->init_time_factor);
  }
  if (game->opts->adaptive_time_factor) {
    pev_write_vl(file, PEV_ADAPTIVE_TIME_FACTOR); pev_write_vl(file, 8);
    pev_write_double(file, game->opts->target_ratio);
  }
  if (game->opts->allow_cheating) { pev_write_vl(file, PEV_CHEAT); pev_write_vl(file, 0); }
  if (game->opts->use_seek) { pev_write_vl(file, PEV_SEEK); pev_write_vl(file, 8); pev_write_double(file, game->opts->seek); }
  pev_write_vl(file, PEV_SPEED); pev_write_vl(file, 8); pev_write_double(file, game->opts->init_speed);
  pev_write_vl(file, PEV_TIME_UNIT); pev_write_vl(file, 8); pev_write_double(file, game->opts->pev_time_unit);
  pev_write_vl(file, PEV_DATE); pev_write_vl(file, 4); pev_write_u32(file, game->start_time.tv_sec);
  pev_write_vl(file, PEV_USER_NAME); pev_write_str(file, game->user_name);
  init_irt = 0;
  if (nev >= 1) init_irt = (int) floor(evs[0].rt / game->opts->pev_time_unit + 0.5);
  if (init_irt > 0) init_irt = 0;
  if (init_irt < 0) {
    pev_write_vl(file, PEV_TIME_OFFSET); pev_write_vl(file, 4); pev_write_u32(file, (unsigned) -init_irt);
  }
  pev_write_vl(file, PEV_HEADER_END);
  last_irt = init_irt;
  for (i = 0; i < nev; i++) {
    InputEvent *ev = &evs[i];
    int irt = (int) floor(ev->rt / game->opts->pev_time_unit + 0.5);
    int idt = irt - last_irt;
    g_assert(idt >= 0);
    pev_write_vl(file, ((ev->is_keypress) ? PEV_EV_PRESS : PEV_EV_RELEASE) + ev->key);
    pev_write_vl(file, idt);
    last_irt = irt;
  }
  pev_write_vl(file, PEV_EV_END);
  fclose(file);
 no_save:
  g_free(evs);
}

static void run_vos_game(const GameOptions *opts, const InputEvent *in_evs, unsigned in_nev,
			 const unsigned char *demo_song_hash, gboolean demo_song_has_hash)
{
  VosGame *game;
  GladeXML *gx;
  unsigned key;
  GtkWidget *main_win;
  unsigned i;
  int result;
  PangoAttrList *attr_list;
  PangoAttribute *attr;
  GameScore sc;
  static const double tscale = 8.0; /* amount that the marker bubbles expand per actual second */

  game = g_new0(VosGame, 1); game->opts = g_new(GameOptions, 1); *game->opts = *opts;
  game->user_name = g_get_user_name(); game->frame_time = 1.0 / game->opts->fps;
  game->time_factor = game->opts->init_time_factor; game->speed_rt = game->opts->init_speed / game->opts->init_time_factor;
  game->tscale_rt = tscale / game->opts->init_time_factor;
  game->in_evs = in_evs; game->in_nev = in_nev; game->volume = -1.0; /* Force volume reset */

  /* Initialize sound */
  result = snd_seq_open(&game->seq, "default", SND_SEQ_OPEN_OUTPUT, SND_SEQ_NONBLOCK);
  g_assert(result == 0);
  game->seq_client_id = snd_seq_client_id(game->seq);
  game->seq_port_id = snd_seq_create_simple_port(game->seq, "PMG output", SND_SEQ_PORT_CAP_READ,
						 SND_SEQ_PORT_TYPE_MIDI_GENERIC);
  g_assert(game->seq_port_id >= 0);
  result = sscanf(game->opts->midi_port_spec, "%u:%u", &game->dest_client_id, &game->dest_port_id);
  g_assert(result == 2);
  g_message("Using MIDI sequencer port: %u:%u", game->dest_client_id, game->dest_port_id);
  result = snd_seq_connect_to(game->seq, game->seq_port_id, game->dest_client_id, game->dest_port_id);
  g_assert(result == 0);
  
  /* Read song */
  game->song = read_vos_file(game->opts->vos_fname, game->opts->tempo_factor);
  if (game->opts->mode == VosGameModePlayDemo && demo_song_has_hash)
    if (memcmp(demo_song_hash, game->song->hash, SHA_DIGEST_LENGTH) != 0)
      g_warning("The SHA-1 hashes do not match.  The demo is probably recorded with a different song.");

  /* Build the user interface */
  gx = glade_xml_new(glade_fname, "main_window", NULL); g_assert(gx != NULL);
  glade_xml_signal_autoconnect(gx);

  /* Initialize the game */
  GET_WIDGET(gx, main_win, "main_window", GTK_WIDGET);
  GET_WIDGET(gx, game->main_area, "main_area", GTK_WIDGET);
  GET_WIDGET(gx, game->state_label, "state_label", GTK_LABEL);
  GET_WIDGET(gx, game->time_label, "time_label", GTK_LABEL);
  GET_WIDGET(gx, game->score_label, "score_label", GTK_LABEL);
  GET_WIDGET(gx, game->rate_label, "rate_label", GTK_LABEL);
  GET_WIDGET(gx, game->time_factor_label, "time_factor_label", GTK_LABEL);
  attr_list = pango_attr_list_new();
  attr = pango_attr_family_new("sans-serif"); attr->start_index = 0; attr->end_index = G_MAXINT;
  pango_attr_list_insert(attr_list, attr);
  attr = pango_attr_size_new(18000); attr->start_index = 0; attr->end_index = G_MAXINT; /* 18pt */
  pango_attr_list_insert(attr_list, attr);
  attr = pango_attr_foreground_new(0, 0, 65535); attr->start_index = 0; attr->end_index = G_MAXINT;
  pango_attr_list_insert(attr_list, attr);
  gtk_label_set_attributes(game->state_label, attr_list);
  pango_attr_list_unref(attr_list);
  
  init_colors(game);
  for (key = 0; key < NKEY; key++) {
    game->cur_un_idxs[key] = game->song->first_un_idxs[key];
    game->cur_un_idxs_long[key] = game->song->first_un_idxs_long[key];
  }
  game->sched = new_event_scheduler(); game->recent_scores = g_new0(GameScore, game->opts->recent_scores_nperiod);
  tempoit_init(game->song, &game->it);
  for (i = 0; i < game->song->ntrack; i++) {
    MidiTrack *track = &game->song->tracks[i];
    if (0 < track->nevent) {
      MidiEvent *ev = &track->events[0];
      double rt = tempoit_tt_to_rt(game->song, &game->it, ev->tt);
      sched_event(game->sched, rt, SchedEventMidi, i, 0);
    }
  }
  for (i = 0; i < game->song->num_note_array; i++) {
    NoteArray *arr = &game->song->note_arrays[i];
    if (0 < arr->num_note) {
      Note *note = &arr->notes[0];
      double rt = tempoit_tt_to_rt(game->song, &game->it, note->tt_start);
      sched_event(game->sched, rt, SchedEventNoteOn, i, 0);
    }
  }
  if (game->opts->mode == VosGameModePlayDemo && 0 < game->in_nev) {
    const InputEvent *ev = &game->in_evs[0];
    sched_event(game->sched, ev->rt, SchedEventDemo, 0, 0);
  }
  sched_event(game->sched, 0.0, SchedEventRefreshScore, 0, 0);
  if (game->opts->mode == VosGameModeAuto) {
    for (i = 0; i < NKEY; i++) {
      unsigned idx = game->song->first_un_idxs[i];
      if (idx != IDX_NONE)
	sched_event(game->sched, game->song->user_notes[idx].rt_start, SchedEventAutoOn, i, idx);
    }
  }  
  for (i = 0; i < NKEY; i++) {
    KeyState *ks = &game->keys[i];
    ks->key_down = FALSE; ks->un_idx = IDX_NONE;
    ks->cmp_press.count0 = 0; ks->cmp_press.score0 = 0.0;
    ks->cmp_press.nent = 0; ks->cmp_press.ents = NULL;
    ks->cmp_release.count0 = 0; ks->cmp_release.score0 = 0.0;
    ks->cmp_release.nent = 0; ks->cmp_release.ents = NULL;
    ks->long_score = 0.0; ks->total_long_score = 0.0;
    ks->markers = g_queue_new();
  }
  game->input_events_arr = g_array_new(FALSE, FALSE, sizeof(InputEvent));

  /* Start the game */
  g_get_current_time(&game->start_time); g_time_val_add(&game->start_time, 3000000); /* Start 3secs later */
  game->is_paused = FALSE; game->start_rt = game->opts->use_seek ? game->opts->seek : game->song->min_rt;
  game->is_resync = TRUE; set_master_volume(game);
  if (game->opts->enable_ui) sched_event(game->sched, get_rt(game), SchedEventRefreshScreen, 0, 0);
  game->event_timeout_id = gtk_timeout_add(0, on_event_timeout, game); /* Trigger process_events() as soon as we run gtk_main() */
  game->conn_key_press = g_signal_connect(G_OBJECT(game->main_area), "key-press-event",
					  G_CALLBACK(on_main_area_key), game);
  game->conn_key_release = g_signal_connect(G_OBJECT(game->main_area), "key-release-event",
					    G_CALLBACK(on_main_area_key), game);
  game->conn_expose = g_signal_connect(G_OBJECT(game->main_area), "expose-event",
				       G_CALLBACK(on_main_area_expose), game);
  game->conn_delete = g_signal_connect(G_OBJECT(main_win), "delete-event",
				       G_CALLBACK(on_main_window_delete), game);
  update_game_state(game); update_game_status(game); update_game_score(game, get_rt(game)); update_time_factor_label(game);
  if (game->opts->enable_ui) {
    gtk_window_set_position(GTK_WINDOW(main_win), GTK_WIN_POS_CENTER);
    gtk_widget_show(main_win);
  }
  gtk_main();

  refresh_scores(game, get_rt(game)); get_game_score(game, &sc);
  g_message("Final score: %0.2f / %0.2f (%0.2f%%)", sc.score, sc.total_score,
	    (sc.total_score == 0.0) ? 100.0 : (sc.score / sc.total_score * 100.0));
  if (game->opts->adaptive_time_factor) g_message("Final time factor: %0.4f", game->time_factor);
  if (game->event_timeout_id) { gtk_timeout_remove(game->event_timeout_id); game->event_timeout_id = 0; }
  g_signal_handler_disconnect(G_OBJECT(main_win), game->conn_delete);
  g_signal_handler_disconnect(G_OBJECT(game->main_area), game->conn_expose);
  g_signal_handler_disconnect(G_OBJECT(game->main_area), game->conn_key_release);
  g_signal_handler_disconnect(G_OBJECT(game->main_area), game->conn_key_press);

  /* Save and free the event list */
  save_input_events(game, game->opts->pev_out_fname);
  
  for (i = 0; i < NKEY; i++) {
    KeyState *ks = &game->keys[i];
    GQueue *markers = ks->markers;
    Marker *m;

    while ((m = (Marker *) g_queue_pop_head(markers)) != NULL) g_free(m);
    g_queue_free(markers);
    g_free(ks->cmp_press.ents); g_free(ks->cmp_release.ents);
  }
  g_free(game->recent_scores); free_event_scheduler(game->sched);
  g_free(game->colors);
  g_object_unref(G_OBJECT(gx));
  free_vos_song(game->song);
  result = snd_seq_close(game->seq); g_assert(result == 0);
  g_free(game->opts); g_free(game);
}

int main(int argc, char **argv)
{
  poptContext popt_ctx;
  InputEvent *in_evs = NULL;
  unsigned in_nev = 0;
  unsigned char demo_song_hash[SHA_DIGEST_LENGTH];
  gboolean demo_song_has_hash = FALSE;
  int result;
  
  gtk_init(&argc, &argv);
  glade_init();

  /* Parse command-line options */
  popt_ctx = poptGetContext("pmg", argc, (const char **) argv, popt_table, 0);
  while ((result = poptGetNextOpt(popt_ctx)) > 0) {
    if (result == 'i') { /* We load the options immediately */
      GArray *in_arr = g_array_new(FALSE, FALSE, sizeof(InputEvent));
      load_input_events(defopts.pev_in_fname, in_arr, demo_song_hash, &demo_song_has_hash, &defopts);
      in_nev = in_arr->len; in_evs = (InputEvent *) g_array_free(in_arr, FALSE);
      defopts.mode = VosGameModePlayDemo;
    } else if (result == 'A') defopts.mode = VosGameModeAuto;
    else if (result == 'S') defopts.enable_ui = FALSE; /* sound only */
    else if (result == 'k') defopts.use_seek = TRUE;
    else if (result == 'a') defopts.adaptive_time_factor = TRUE;
    else if (result == 'v') verbose = TRUE;
    else g_assert_not_reached();
  }
  g_assert(result < 0);
  if (result < -1) {
    fprintf(stderr, "%s: %s\n", poptBadOption(popt_ctx, POPT_BADOPTION_NOALIAS), poptStrerror(result));
    exit(1);
  }
  defopts.vos_fname = poptGetArg(popt_ctx);
  if (defopts.vos_fname == NULL) { poptPrintUsage(popt_ctx, stderr, 0); exit(1); }
  poptFreeContext(popt_ctx);
  if (defopts.fast_mode) { defopts.enable_ui = FALSE; defopts.enable_sound = FALSE; }

  run_vos_game(&defopts, in_evs, in_nev, demo_song_hash, demo_song_has_hash);
  
  if (in_evs) g_free(in_evs);
  return 0;
}
