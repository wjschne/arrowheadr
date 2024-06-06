## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

As recommended in a note from Beni Altmann on 2024-06-06, I have made the following changes since the previous submission:

* I have added more details in the Description field at about what the package does.
* In the Description field, I have enclosed the names of packages in single quotes.
* I used the `on.exit` function as recommended to reset the user's `par` when the special plotting function is exited.
* I removed a change to the user's `par` in one of the examples.
