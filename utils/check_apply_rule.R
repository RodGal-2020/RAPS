library(RAPS)
my_rap = RAPS::path2rap(demo = 2)

# We have the following rules:
#
# 1. a --> b
# 2. a --> b*2
# 3. a --> b*2, c
# 4. b*2 --> c
# 5. a, b --> c
# 6. a, b*2 --> c
# 7. a, b\*2 --> c\*3
# 8. a, b\*2 --> c\*3, d\*4
# 9. a --> lambda
# 10. [a [ ]'2 --> [a]'2]'1
# 11. [ [a]'2 --> a [ ]'2]'1
# 12. a --> NEW

i = 1
cat("i = ", i )
RAPS::show_rule(rules[i, ])
my_new_rap = my_rap %>%
  apply_rule(rule_id = i) %$%
  RAP

for (membrane in 1:3) {
  cat("\n\nIn a membrane labelled with ", membrane - 1, " we have...\n")
  print(my_new_rap$objects[[membrane]])
  cat("----------------------------------")
  cat("\n----------------------------------")
}
