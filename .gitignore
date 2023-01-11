 ALTER TABLE evictions ALTER COLUMN filings_2020 DECIMAL (5,2)

 ALTER TABLE evictions ALTER COLUMN GEOID NUMERIC (38,0) 


  --Average Evictions per month in 2020
SELECT  month, AVG(filings_2020) AS avg_evictions
 FROM evictions
 GROUP BY  month
 ORDER BY avg_evictions DESC

  --Showing census tracts with higher than average evictions filings in January
SELECT geoid, racial_majority, filings_2020
 FROM evictions 
 WHERE month='Jan-20' AND filings_2020>11
 GROUP BY geoid, racial_majority, filings_2020
 order by filings_2020 desc

  --Average evictions and census tract racial majority 
SELECT Geoid, racial_majority, AVG(filings_2020) AS avg_evictions
 FROM evictions
 GROUP BY geoid, racial_majority
 ORDER BY avg_evictions DESC

	--Average evictions by race
 Select racial_majority, AVG(filings_2020) AS avg_evictions
 from evictions
 group by racial_majority 
 order by avg_evictions desc

  --Average evictions inner joined with household_char (more than 1.5 occupants per room)
SELECT Geoid, morethan1_5_occupants_per_room, AVG(filings_2020) AS avg_evictions
 FROM evictions 
   join Household_char
 ON GEOID=household_char.census_tract
 GROUP BY GEOID, morethan1_5_occupants_per_room
 ORDER BY morethan1_5_occupants_per_room DESC
