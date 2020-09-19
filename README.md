# covid_exposure

Comparing numbers of keys downloaded daily by Scotland's COVID Exposure Notification [app](https://protect.scot) to the number of new daily cases.

The number of keys downloaded should be the number of positive cases who have chosen to log their positive test via the app. 

Note there will likely be different time lags involved between the two quantities, and a lack of completeness in most recent plotted time periods.

Also note that different phones / operating systems etc may download new keys more or less frequently than others. My iPhone seems to poll the server and download 2 - 4 times daily, so it makes sense to present the data aggregated to daily level. Exposure notification files generated by different phones / operating systems may have different filenames and structures, and the code presented here works for files exported from iPhones. However, it should be easily adaptable.

![](/pics/plot_cases_keys.png)

![](/pics/plot_cases_keys.png)
