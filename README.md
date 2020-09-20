# covid_exposure

This repository contains **R** code that compares the number of *diagnosis keys* downloaded by Scotland's COVID-19 exposure notification [app](https://protect.scot) to the number of daily COVID-19 cases. The number of downloaded keys is extracted from files exported via the iPhone "Exposure Notifications" settings pane, and the number of new daily COIVD cases come from Public Health Scotland's [Open Data portal](https://www.opendata.nhs.scot). 

## Background

My rough understanding (and I'm very happy to be corrected here!) is that Bluetooth data is exchanged by passing devices using a "rolling proximity identifier" (which changes ~15 minutes) which is coupled with a "temporary exposure key" (which changes daily). If a user self-reports as a positive case, the (~14) most recent temporary exposure keys are bundled as a single "diagnosis key" and uploaded to a central (NHS) server. New diagnosis keys are periodically downloaded by individual user's phones; and each phone then checks whether the identifiers it has collected from other phones in its vicinity match any of these diagnosis keys (and issues a self-isolation warning if so).

Some useful links that outline this are:
* [coronawarn app's FAQ on export file definitions](https://www.coronawarn.app/en/faq/#keys_matches)
* [Protect Scot App "How It Works" page](https://protect.scot/how-it-works)
* [Google's Exposrure Notifications site](https://www.google.com/covid19/exposurenotifications/)
* [Apple's Exposure Notifications site](https://covid19.apple.com/contacttracing)
* [Apple/Google Exposure Notification FAQ doc](https://static.googleusercontent.com/media/www.google.com/en//covid19/exposurenotifications/pdfs/Exposure-Notification-FAQ-v1.2.pdf)
* [Bluetooth Specifaction doc](https://blog.google/documents/70/Exposure_Notification_-_Bluetooth_Specification_v1.2.2.pdf)

## Exposure Notification Export Files

Both iPhone and Android OSs allow users to export metadata files related to recently downloaded diagnosis keys. Note, this is *not* the keys themselves, but simply a note of the *number* of keys downloaded, a time-stamp of when they were downloaded, and the number that match with those already recorded by the user's phone. This "match count" will be zero if the user's phone has not been in the vicinity of someone who later self-reported as a positive case. (However, it's not clear to me if a non-zero match count indicates *any* level of proximity, or if it indicates proximity *significant enough* for the user to be alerted e.g. <2m for 15m)

So, the number of *new keys* downloaded by the app refers to the number of cases who have self-reported their positive test via the app, and as such it could be interesting to compare this to the *reported number of new cases*. This might give some insight as to how widely used the app is: if the number of new cases is the same as the number of new downloaded keys, it implies everybody who knows they're positive has self-reported using the app.

### Caveats etc

* Note there will likely be different time lags involved (people might not *immediately* self-report, and the new cases data is reported by the date the test was performed), and a lack of completeness in the most recent time periods.

* My iPhone typically polls the server to download new keys 2 - 4 times a day, so the plots below are aggregated to daily level for ease of comparison to the daily "new cases" data. Other phones may download larger batches of new keys but do see less often, in which case aggregating to daily level may cause intermittent blank periods etc.

* Exposure notification files generated by different phones / operating systems may have different filenames and structures. The code here is written with iPhone output files in mind (because that's what I have to hand). There is an option to use files exported from an Android device, but this has only been tested sparingly! However, the files are not complex, so the code should be easy enough to adapt.

## New Daily Cases / Keys
Here we can see the daily number of downloaded keys compared to the daily COVID-19 cases. It's early days yet but it looks like around 40% of new cases are currently being registered via the app.

![](/pics/plot_cases_keys.png)

## Cumulative Cases / Keys
Here's the cumulative count of cases and keys downloaded (from the first day of data I have, September 10th). Note that this covers the early growth phase of the app, so it may be more instructive to look at more recent cumulative data.

![](/pics/plot_cum_cases_keys.png)

## Cumulative Cases / Keys Since September 17th
The app reached one million downloads on September 17th, and starting from then we can see that about 3/5 new cases have been registered. Although note that as this is only a few days worth of data (and relative lag may be of the order of a few days), this is quite speculative.

![](/pics/plot_cum_cases_keys_sep17.png)


## Conclusion

Cautious optimism! It looks like a significant proportion of known cases are self-reporting via the app, which can only be a help to the general contact tracing efforts.

