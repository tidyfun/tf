## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

--------------------------------------------------------------------------------

RE: man/tfb_fpc.Rd l.124: "scoring_function = tf:::.fpc_wsvd_scores"

The advanced, "donttest"-example here makes use of an un-exported function from 
the package via ":::". It is only used here to show experts how to extend the 
package, but the function being used is NOT part of the package's user 
interface, and other developers will have to re-implement a "scoring_function" 
specific for their own method anyway. So it would not make sense to export 
".fpc_wsvd_scores", modifying the example so that it works without using the 
triple-colon would make it rather long and much harder to understand, and 
removing the example would mean the extension mechanism we provide is not 
documented well.
Please let us use ":::" in this case.

--------------------------------------------------------------------------------
