library(RAPS)
my_rap = RAPS::path2rap(demo = 2)
rules = my_rap$Rules

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


with_rule = function(index) {
  i = index
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
}

# a --> b
with_rule(1)

# a --> b*2
with_rule(2)

# a --> b*2, c
with_rule(3)

# b*2 --> c
with_rule(4)

# a, b --> c
with_rule(5)

# a, b*2 --> c
with_rule(6)

# a, b\*2 --> c\*3
with_rule(7)

# a, b\*2 --> c\*3, d\*4
with_rule(8)

# a --> lambda
with_rule(9)

# [a [ ]'2 --> [a]'2]'1
with_rule(1)

# [ [a]'2 --> a [ ]'2]'1
with_rule(1)

# a --> NEW
with_rule(1)
