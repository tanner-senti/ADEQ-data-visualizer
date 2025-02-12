# ADEQ-data-visualizer
This is an unofficial project in active development

Pull data from Arkansas DEQ's Water Quality Monitoring Database and plot. Database is publicly available as an .mdb file from https://www.adeq.state.ar.us/techsvs/env_multi_lab/water_quality_station.aspx, thus hosting this app on a linux server (like shinyapps.io) will cause errors and default to a backup Duck.db database.

"app.R" is a working version of the app to pull data directly from the ADEQ SQL Server. This will only work if the app is hosted on ADEQ servers.
"app-backup-access.R" is a working version to download the publicly available database, and will work locally on windows machines but not on linux servers.

See an example of this app deployed at:  https://i24os8-tanner-senti.shinyapps.io/ADEQ-data-visualizer/
