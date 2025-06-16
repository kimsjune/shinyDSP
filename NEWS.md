# shinyDSP 1.0.3

## RELEASE_3_21 fix

* v1.0.2 reflected partial bug fixes for peer review. This is because I didn't 
`git cherry-pick` **a range** of commit hashes from devel. The commit history is
thus all over the place, but the content should be the same. 


# shinyDSP 1.0.2



## Bug fixes for peer review

* in the setup nav panel, count and annotation tables actually show the first 
100 rows.
* added custom BH-adjusted P value input.
* removed 'turbo' as an option for heatmap colormap because it's not a part of 
hcl.colors(). Instead made all 115 palettes in hcl.colors() available for 
selection.
* updated the secondary vignette to show RUV4 normalization, differential gene
expression analysis, plotting Volcano and heatmaps, and code chunk execution 
times.



# shinyDSP 0.99.0

## NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.
