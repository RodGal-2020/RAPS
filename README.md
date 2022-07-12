# RAPS: R Aid for P systems

## Status
**General objectives:**
- [x] Create `README.md`
- [ ] Complete `README.md`


- [ ] Adapt parser to P-Lingua 5
- [ ] Create one or more simulators:
  - [ ] A general simulator
  - [ ] For P-Lingua 5

**Functions**
- [x] Organise the main functions in the "Functions" section

`R/read_xml_p_system.R`
- [ ] Check using of `unnest_wider`, `longer` or `auto` to reduce the use of lists
- [ ] Improve `rhs_multisets` with `process_multiset` and `print_multiset`, just as I did before with `lhs_multisets`

`R/simulate_p_system.R`
- [ ]  Make it work
  - [ ] Begin with basic evolution rules.
  
## Functions

### New ideas
NOMBRE DEL DATAFRAME: NOMBREpt

* `apply_rule`
* `choose_rule`
* `tib2NOMBREpt`
* `df2NOMBREpt`
* `url2NOMBREpt(format = "pl5")`

Ideal order of application:
```{r}
my_NOMBREpt = data.frame %>%
  tib2NOMBREpt()
  
my_NOMBREpt = my_url %>%
  url2NOMBREpt(format = "pl5")
  
selected_rule = my_NOMBREpt %>%
  choose_rule()
  
new_NOMBREpt = my_NOMBREpt %>%
  apply_rule(selected_rule)
```

### Original
* `na_omit`
* `print_multiset`
* `process_multiset`
* `read_ps5`
* `read_xml_p_system`
* `simulate_p_system`
