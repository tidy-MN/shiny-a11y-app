## Accessible penguins

Designed by [Abigail Stamm](https://github.com/ajstamm), Fall 2024

This app illustrates several accessibility features available in Shiny dashboards. It is not entirely cohesive as an app since each tab is designed to illustrate specific features.

## WCAG guidelines illustrate in the app

For specific details on each item, see the [WCAG guidelines at W3.org](https://www.w3.org/WAI/WCAG22/quickref/).

### Text formatting tab

* 1.4.12 Text Spacing (AA)  
* 1.4.4 Resize Text (AA)
* 1.4.3 Text Color Contrast (Minimum) (AA) 

### Plots tabs

* 1.4.1 Non-text Use of Color (A) 
* 1.4.11 Non-Text Color Contrast (AA)
* 1.4.13 Content on Hover or Focus (AA)

### About data tab

* 1.1.1 Non-text Content (A) 

### Individual data tab

* 2.1.1 Keyboard Navigation (A) 
* 2.4.3 Focus Order (A) 
* 2.4.7 Keyboard Focus Visible (AA)

### Sidebar

* 1.3.5 Identify Input Purpose (AA) 
* 3.2.2 Content Change On Input (A) 
* 3.3.2 Labels or Instructions (A)
* 3.2.4 Consistent Identification (AA)

### General navigation

* 1.4.10 Reflow (AA)
* 3.2.3 Consistent Navigation (AA)
* 2.4.2 Page Titled (A) 

### WCAG fails

* 3.3.1 Error Identification (A)
  - Bar chart tab: filter on Gentoo and Dream
    - Failure: textured bar chart displays uninformative error
    - Success: plain bar chart displays instructions
* 1.3.3 Sensory Characteristics (A)
  - Bar chart tab: plotly icons
    - Failure: no non-hover text equivalent and uninformative ARIA text
    - Possible (not ideal) solution: deactivate icons




