#include <glib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

static unsigned vosfile_read_u8(FILE *file)
{
  guint8 tmp;
  int result;

  result = fread(&tmp, 1, 1, file); g_assert(result == 1);
  return tmp;
}

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

static void dump_data(const void *pdata, unsigned len, const char *caption_fmt, ...)
{
  va_list args;
  unsigned i;
  const unsigned char *data = (const unsigned char *) pdata;

  va_start(args, caption_fmt);
  vprintf(caption_fmt, args);
  for (i = 0; i < len; i++) {
    printf("%02x", data[i]);
    putchar((i + 1 == len) ? '\n' : ' ');
  }
  va_end(args);
}

static void vosfile_read_info(FILE *file, unsigned inf_ofs, unsigned inf_len)
{
  int result;
  unsigned inf_end_ofs = inf_ofs + inf_len;
  char *title, *artist, *comment, *vos_author;
  unsigned song_type, ext_type, song_length, level;
  char buf[4];

  result = fseek(file, inf_ofs, SEEK_SET); g_assert(result == 0);
  /* Skip the "VOS1" header in e.g. test3.vos and test4.vos, if any */
  result = fread(buf, 4, 1, file);
  if (result == 1 && memcmp(buf, "VOS1", 4) == 0) { /* Found "VOS1" header */
    char *str1;
    result = fseek(file, 66, SEEK_CUR); g_assert(result == 0);
    str1 = vosfile_read_string(file); printf("Str1: %s\n", str1); g_free(str1);
  } else {
    result = fseek(file, inf_ofs, SEEK_SET); g_assert(result == 0);
  }
  title = vosfile_read_string(file); printf("Title: %s\n", title); g_free(title);
  artist = vosfile_read_string(file); printf("Artist: %s\n", artist); g_free(artist);
  comment = vosfile_read_string(file); printf("Comment: %s\n", comment); g_free(comment);
  vos_author = vosfile_read_string(file); printf("Vos author: %s\n", vos_author); g_free(vos_author);
  song_type = vosfile_read_u8(file); ext_type = vosfile_read_u8(file);
  printf("Song type: %u/%u\n", song_type, ext_type);
  song_length = vosfile_read_u32(file);
  printf("Song length: %0.1fs\n", song_length * 1.024e-3); /* unsure */
  level = vosfile_read_u8(file);
  printf("Difficulty level: Lv%u\n", level + 1);
  
  result = fseek(file, 1023, SEEK_CUR); g_assert(result == 0);
  while (1) {
    unsigned cur_ofs;
    unsigned type, nnote, i;
    char dummy2[14];
    char raw_note[13];
    
    result = ftell(file); g_assert(result != -1); cur_ofs = result;
    if (cur_ofs == inf_end_ofs) break;
    type = vosfile_read_u32(file); nnote = vosfile_read_u32(file);
    printf("Note array at offset 0x%x: type=0x%x nnote=%u\n", cur_ofs, type, nnote);
    result = fread(dummy2, 14, 1, file); g_assert(result == 1);
    dump_data(dummy2, 14, "  dummy2: ");
    for (i = 0; i < nnote; i++) {
      result = fread(raw_note, 13, 1, file); g_assert(result == 1);
      dump_data(raw_note, 13, "  note %4u: ", i);
    }
  }
}

static void vosfile_read_vos1(FILE *in_file, unsigned file_size)
{
  unsigned inf_ofs = 0, inf_len = 0, mid_ofs = 0, mid_len = 0;
  unsigned ofs, next_ofs;
  char *seg_name;

  /* We are now just after the magic part */
  /* Read the segments */
  ofs = vosfile_read_u32(in_file);
  while (1) {
    seg_name = vosfile_read_string_fixed(in_file, 16);
    if (strcmp(seg_name, "EOF") == 0 || ofs == file_size) { g_free(seg_name); break; }
    next_ofs = vosfile_read_u32(in_file);
    if (strcmp(seg_name, "inf") == 0) { inf_ofs = ofs; inf_len = next_ofs - ofs; }
    else if (strcmp(seg_name, "mid") == 0) { mid_ofs = ofs; mid_len = next_ofs - ofs; }
    else g_assert_not_reached();
    g_free(seg_name);
    ofs = next_ofs;
  }
  g_assert(inf_len != 0); g_assert(mid_len != 0);

  vosfile_read_info(in_file, inf_ofs, inf_len);
}

static void vosfile_read_info_022(FILE *file, unsigned subfile_ofs, unsigned subfile_len)
{
  int result;
  char magic[6];
  char *title, *artist, *comment, *vos_author, *str, *lyric;
  char unknown1[11], raw_note[16];
  unsigned song_length, song_length_tt;
  unsigned narr, nnote, nunote, nlyric, x, type, level, key, time;
  unsigned i, j, k, cur_ofs;
  unsigned version;
  
  /* We are now at the beginning of the contents of the .trk subfile */
  result = fread(magic, 6, 1, file); g_assert(result == 1);
  if (memcmp(magic, "VOS022", 6) == 0) version = 22;
  else if (memcmp(magic, "VOS006", 6) == 0) version = 6;
  else g_assert_not_reached();
  
  title = vosfile_read_string2(file); printf("Title: %s\n", title); g_free(title);
  artist = vosfile_read_string2(file); printf("Artist: %s\n", artist); g_free(artist);
  comment = vosfile_read_string2(file); printf("Comment: %s\n", comment); g_free(comment);
  vos_author = vosfile_read_string2(file); printf("Vos author: %s\n", vos_author); g_free(vos_author);
  str = vosfile_read_string2(file); printf("Str: %s\n", str); g_free(str);
  result = fread(unknown1, 11, 1, file); g_assert(result == 1); dump_data(unknown1, 11, "Unknown1: ");
  song_length_tt = vosfile_read_u32(file); song_length = vosfile_read_u32(file);
  printf("Song length: %0.1fs (0x%x in tt(?), 0x%x in rt)\n", song_length * 1.024e-3, song_length_tt, song_length); /* unsure */
  
  result = fseek(file, 1024, SEEK_CUR); g_assert(result == 0);
  narr = vosfile_read_u32(file); x = vosfile_read_u32(file); g_assert(x == 1);
  printf("Number of note arrays: %u\n", narr);
  for (k = 0; k < narr; k++) {
    x = vosfile_read_u8(file); g_assert(x == 4);
    type = vosfile_read_u32(file); printf("Note array %u: type=0x%x\n", k, type);
  }
  x = vosfile_read_u8(file); g_assert(x == 0);
  level = vosfile_read_u8(file); printf("Difficulty level: %u\n", level + 1);
  str = vosfile_read_string2(file); printf("Str: %s\n", str); g_free(str);
  x = vosfile_read_u32(file); g_assert(x == 0);
  for (k = 0; k < narr; k++) {
    result = ftell(file); g_assert(result != -1); cur_ofs = result;
    nnote = vosfile_read_u32(file);
    printf("Note array %u: cur_ofs=0x%x, nnote=%u\n", k, cur_ofs, nnote);
    for (i = 0; i < nnote; i++) {
      result = fread(raw_note, 16, 1, file); g_assert(result == 1);
      dump_data(raw_note, 16, "  note %4u: ", i);
    }
  }
  if (version == 22) { x = vosfile_read_u32(file); g_assert(x == 0); } nunote = vosfile_read_u32(file);
  printf("User note array: nunote=%u\n", nunote);
  for (j = 0; j < nunote; j++) {
    k = vosfile_read_u8(file); g_assert(k < narr);
    i = vosfile_read_u32(file);
    key = vosfile_read_u8(file); g_assert(key < 7);
    printf("Note %u: arr=%u idx=%u key=%u\n", j, k, i, key);
  }
  x = (version == 22) ? vosfile_read_u32(file) : 0;
  if (x == 0) {
    nlyric = vosfile_read_u32(file);
    printf("Lyric array: nlyric=%u\n", nlyric);
    for (i = 0; i < nlyric; i++) {
      time = vosfile_read_u32(file);
      lyric = vosfile_read_string2(file);
      printf("Lyric at tt=0x%x: %s\n", time, lyric);
      g_free(lyric);
    }
  } else printf("Unknown x=%u\n", x);
  result = ftell(file); g_assert(result != -1); cur_ofs = result;
  printf("Offset=0x%x, remaining bytes: %u\n", cur_ofs, (subfile_ofs + subfile_len) - cur_ofs);
}

static void vosfile_read_vos022(FILE *in_file, unsigned file_size)
{
  int result;
  unsigned subfile_idx = 0, ofs = 4, data_ofs; /* The offset of the current subfile */
  unsigned fname_len, len;
  char *fname;

  while (1) {
    if (ofs == file_size) break;
    result = fseek(in_file, ofs, SEEK_SET); g_assert(result == 0);
    fname_len = vosfile_read_u32(in_file);
    fname = vosfile_read_string_fixed(in_file, fname_len);
    len = vosfile_read_u32(in_file); data_ofs = ofs + 4 + fname_len + 4;
    printf("File %s at offset 0x%x\n", fname, data_ofs);
    if (subfile_idx == 0) {
      g_assert(strcmp(fname, "Vosctemp.trk") == 0);
      vosfile_read_info_022(in_file, data_ofs, len);
    } else g_assert(strcmp(fname, "VOSCTEMP.mid") == 0);
    g_free(fname);
    ofs = data_ofs + len; subfile_idx++;
  }
}

int main(int argc, char **argv)
{
  const char *in_fname;
  FILE *in_file;
  unsigned file_size, magic;
  int result;
  
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <file.vos>\n", argv[0]);
    return 1;
  }
  in_fname = argv[1];
  in_file = fopen(in_fname, "rb"); g_assert(in_file != NULL);

  result = fseek(in_file, 0, SEEK_END); g_assert(result == 0);
  file_size = ftell(in_file);
  result = fseek(in_file, 0, SEEK_SET); g_assert(result == 0);
  magic = vosfile_read_u32(in_file);
  if (magic == 3) vosfile_read_vos1(in_file, file_size);
  else if (magic == 2) vosfile_read_vos022(in_file, file_size);
  else g_assert_not_reached();

  fclose(in_file);
  return 0;
}
