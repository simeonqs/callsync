## Reviewer comments

* Please write TRUE and FALSE instead of T and F. Please don't use "T" or "F" as vector names.

This was fixed in all functions. 

* Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.

This was included in all functions that used par(). 

## R CMD check results

 
* New submission

This is a resubmission where several comments of the reviewer are addressed. 

* Possibly misspelled words in DESCRIPTION: spectrographic (14:10)

This is the correct spelling. 
