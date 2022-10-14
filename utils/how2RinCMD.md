# How to use R in the cmd

1. Use Linux.

2. Make sure that you have R installed, launching the following:

```cmd
  R --version
```

3. Create your first dummy R script, like this one:

```cmd
  nano dummy.R
```

```r
  print("Dummy launched correctly! :D")
```

4. Launch the script using the Rscript utility —which I believe that comes with R—:

```cmc
  Rscript dummy.R
```
  
  You should see the line `Dummy launched correctly! :D`.
