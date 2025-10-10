---
title: "Prelimary AirTable Analysis on ICE Sightings and Canvassing"
subtitle: "From 2.1.25 to 9.28.25"  
output:
  pdf_document: default
---



**I. Differentiation from AirTable Data**

The dashboard in AirTable counts each of the tags and produces a bar chart. Since this chart does considers FALSE and Rumors a classification tag, instead of its a separate variable, it unintentionally overstates the I-9 audits, incidents of corporate collaboration, worksite raids, home raids, public space raids, and ICE sightings that have occurred up to 9.29.

For example, the visualization includes 970 ICE sightings, but this count includes sightings that have later been proven False ("Ice Sighting, False"). If we actually break down ICE sightings by whether or not they are true, false, or unproven, we find that only 272 have been proven true, 95 are confirmed false, 441 were unable to be confirmed.


Table: Type of Incident by Report Validity (All Reports)

|refined_categories | Unconfirmed| Confirmed False| Confirmed True| percentage_confirmed_true|
|:------------------|-----------:|---------------:|--------------:|-------------------------:|
|Corp Collab        |           2|               1|              0|                 33.333333|
|Home Raid          |           6|               3|             52|                  4.918033|
|I-9 Audit          |           0|               0|              3|                  0.000000|
|Ice Sighting       |         441|              95|            272|                 11.757426|
|No tags            |         130|             111|              1|                 45.867769|
|Pub Space Raid     |          35|               2|            121|                  1.265823|
|Unclass. Raid      |           0|               0|              1|                  0.000000|
|Worksite Raid      |           5|               0|              9|                  0.000000|

![](false_reports_analysis_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

**II. Analysis of Activities Verified by RRT**

When we analyze incidents where a rapid responder was dispatched (619/1290 cases), we can see that the overall distribution looks similar to the broader data set.

![](false_reports_analysis_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

Table: Type of Incident by Report Validity (Only Reports Verified by Rapid Responders)

|refined_categories | Unconfirmed| Confirmed False| Confirmed True| percentage_confirmed_true|
|:------------------|-----------:|---------------:|--------------:|-------------------------:|
|Corp Collab        |           0|               1|              0|                100.000000|
|Home Raid          |           1|               2|             45|                  4.166667|
|I-9 Audit          |           0|               0|              3|                  0.000000|
|Ice Sighting       |          89|              73|            166|                 22.256098|
|No tags            |          26|              93|              0|                 78.151261|
|Pub Space Raid     |           9|               2|            101|                  1.785714|
|Unclass. Raid      |           0|               0|              1|                  0.000000|
|Worksite Raid      |           1|               0|              6|                  0.000000|

Some notable exceptions exist. For example, false incidents comprise **12.5%** of total ICE sightings, but where a rapid responder was dispatched, incidents were proven false **22.3%** of the time. 

A two-proportion z-test indicates that ICE Sightings verified by rapid responders were  significantly more likely to be disproven (22.3%) than those reported overall (11.8%, p < 0.001).

This suggests that the AirTable dashboard may understate the frequency of false reports, and that the ‘true’ accuracy of ICE Sightings is likely closer to this 22% false-report threshold.


```r
# Set up for our statistics test 
false_counts <- c(95, 73)
total_counts <- c(808, 328)

# Run two-proportion z-test
prop.test(false_counts, total_counts, alternative = "less", correct = FALSE)
```

```
## 
## 	2-sample test for equality of proportions without continuity correction
## 
## data:  false_counts out of total_counts
## X-squared = 20.406, df = 1, p-value = 3.132e-06
## alternative hypothesis: less
## 95 percent confidence interval:
##  -1.00000000 -0.06286024
## sample estimates:
##    prop 1    prop 2 
## 0.1175743 0.2225610
```


**III. Geographic Distribution of Reports**

The majority of reports are clustered along the southwest side:



Reports -- regardless of their validity -- are concentrated along the southwest side. 

![](false_reports_analysis_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

Confirmed reports are clustered in the following neighborhoods:

![](false_reports_analysis_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

False reports are clustered in the following neighborhoods:

![](false_reports_analysis_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

When determining the neighborhoods to target for canvassing and KYR trainings, looking at the number of false ICE sightings is important, but also, should be considered in context. We can compare the number of false reports against a neighborhood's total immigrant population. 

*This comparison assumes this reporting is driven by communities that are most likely to be impacted by ICE, and that gaps in reporting reflect awareness gaps -- not a lack of ICE.* 

![](false_reports_analysis_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 


**IV. Interpretation and Recommendations**

1.  ICE Sightings are by far the common type of the incident. Thus far, the network has recorded 272 confirmed ICE Sightings, 121 Public Space Raids, 52 Home Raids, 9 work site raids, and 6 I-9 raids.

2.  Nearly 1 in 4 Ice Sightings were proven false by a rapid responder (22.3%). In contrast, only 2.0% percent of public space raids were proven false and 4.4% of home raids were proven false by a responder.

    Considering this large discrepancy in valid ICE Sightings -- a gap that raises to 49.4% when we add in sightings that were unable to be validated -- I would recommend the FSN implement a triage system. One potential suggestion is to have a FSN dispatcher query the license plate database and search for a potential match before calling for a responder.

    Would also ask leadership to consider the following question: Is every sighting worth validating, especially in times when we are hurting for volunteer capacity? Is there a threshold that needs to be met for passing along a sighting?

3.  In terms of the geographic distribution of reports, have a couple of takeaways I'd like to stress: A. High numbers of false reports are, in some ways, a good thing. That means people have utilized the ICIRR hotline and understand it's a resource (likely what we are seeing in Evanston, downtown, Little Village, and Uptown). In this case, it is safe to say that they are aware of the hotline's existence -- they just likely need more KYR training.

    Would recommend the Little Village team, in particular, do more KYR trainings since this neighborhood has been hit particularly heavily and also has a high quantity of false reports.

4.  Lack of data on reports can be equally important to understanding a community. If we look at the South Side, we can see that there are a low quantity of reports to begin with -- and almost no false sightings. 

Would recommend the Far South Team focus on distributing literature first to raise awareness for the FSN, then move into more capacity intensive KYR workshops. 
