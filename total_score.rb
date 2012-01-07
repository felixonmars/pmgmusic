#!/usr/bin/env ruby
sum_score = 0.0 ; sum_total = 0.0
$stdin.each_line do |line|
  if line =~ %r{^[ 0-9]+: ([ 0-9.]+) / ([ 0-9.]+) }
    cur_score = Float($1) ; cur_total = Float($2)
    sum_score += cur_score ; sum_total += cur_total
  else throw Exception.new("Unrecognized line") end
end
printf "%0.2f / %0.2f (%0.2f%%)\n", sum_score, sum_total, 100.0 * sum_score / sum_total
