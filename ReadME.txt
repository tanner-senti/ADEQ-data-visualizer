Tanner Senti 
2025-02-27

App Usage:
Simply make sure all packages are installed, then click "Run App"
in the top-right corner of RStudio.

The standard app will connect to the WebLIMS SQL Server database and interactively
plot selected parameters.

This method will work if the app is:
	- Run locally on a laptop connected to DEQ IT's servers (all employee laptops)
	- If the app is hosted by DEQ to a webpage in the future

There is a backup script "app-backup-access.R" that functions the exact same,
except this version will download the entire WebLIMS database Access file that is
publicly available on DEQ's website.

This alternative method will work, however it is much slower. Note that this method:
	- Will work on any windows laptop, even if not connected to DEQ IT's servers
		-i.e. you can run this app on your personal laptop at home
	- Will NOT work if hosted (linux cannot work with Access files)

NOTE:
If there is an error downloading the data, the app will revert to a backup database.
This database is just a small selection of data (~2016 - 2024) so that the app still
functions if the download fails. Errors will need to be investigated if this occurs.