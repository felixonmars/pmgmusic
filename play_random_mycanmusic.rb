#!/usr/bin/ruby

Dir.chdir "#{ENV['HOME']}/test/vos"
while true
  num = rand(7571)
  fname = "/dos/d/MyCanMusic/CanFile/All/#{num}.vos"
  puts fname
  system "./pmg", fname, "-ASv"
  sleep 1 # so that pressing Ctrl-C twice terminates the program
end
