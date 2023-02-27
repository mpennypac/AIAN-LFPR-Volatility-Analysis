import pandas as pd
import numpy as np
import re, struct

"""state_abb = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
             "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
             "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"]

data = pd.DataFrame()
#aian_lfpr = pd.read_csv('AIAN Labor Force Participation Rate.csv')
#data['AIAN'] = aian_lfpr[aian_lfpr.columns[1]]

for state in state_abb:
    df = pd.read_csv("{0} LFPR.csv".format(state))
    df = df.drop('DATE',axis=1)
    df = df.rename(columns={df.columns[0]:'{0}'.format(state)})
    data[state] = df

data.to_csv('state-level-LFPR.csv',index=False)"""

months = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
#months = ['dec','nov','oct','sep','aug','jul','jun','may','apr','mar','feb','jan']
years = ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16',
         '17', '18', '19', '20', '21', '22']

"""
## testing block
filename = '{0}{1}pub.csv'.format('dec', '22')
data = pd.read_csv(filename)
data = data[data['ptdtrace'] == 3].reset_index(drop=True)
avg_age = np.mean(data['prtage'])
age_survey_ct = len(data['prtage'])
#age_missing_ct = len(data[data['prtage'] == ''])
print(data.columns)
print(avg_age, age_survey_ct)#, age_missing_ct)
"""

#lines = data.read()
"""numlines = 0
for line in data:
    lines = line
    print(lines)
    print(type(lines))
    print(len(lines))
    numlines += 1
print(numlines)
data.close()"""

#compiled_data = pd.DataFrame()
compiled_data = pd.DataFrame()
#compiled_data = compiled_data.drop([compiled_data.columns[0], compiled_data.columns[1]],axis=1)
#compiled_data.to_csv('compiled-data.csv',index=False)
count = len(compiled_data) + 1
#for yr in years:
yr = '00'
for mon in months:
    compiled_data.loc[count, 'Time'] = '{0}{1}'.format(mon, yr)

    if (yr == '12' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']) or (yr in ['22', '21', '20', '19', '18', '17', '16', '15', '14', '13']):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'prtage'
    elif (yr == '12' and mon in ['jan', 'feb', 'mar', 'apr']) or (yr in ['11', '10', '09', '08', '07', '06']) or (yr == '05' and mon in ['aug', 'sep', 'oct', 'nov', 'dec']):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'peage'
    elif (yr == '04' and mon in ['jan', 'feb', 'mar', 'apr']) or (yr == '03'):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'prtage'
    elif (yr == '05' and mon in ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul']) or (yr == '04' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']):
        race_var = 'prdtrace'
        aian_val = 3
        age_var = 'peage'
    elif (yr in ['02', '01', '00']):
        race_var = 'perace'
        aian_val = 3
        age_var = 'prtage'

    earn_var = 'PTERNH2'
    gend_var = 'pesex'
    male_val = 1
    female_val = 2

    ## data compilation format for years 2000-2019 (weird ascii file type)
    if yr in ['19', '18', '17', '16', '15', '14', '13', '12', '11', '10', '09', '08', '07', '06', '05', '04', '03', '02', '01', '00']:
        ## data dictionary file for years 2017-2019
        #data_dict = open('January_2017_Record_Layout.txt').read()
        ## data dictionary file for years 2015-2016
        #data_dict = open('January_2015_Record_Layout.txt').read()
        ## data dictionary file for year 2014
        #data_dict = open('January_2014_Record_Layout.txt').read()
        ## data dictionary file for year 2013
        #data_dict = open('January_2013_Record_Layout.txt').read()
        ## data dictionary files for year 2012
        #if mon in ['jan', 'feb', 'mar', 'apr']:
        #    data_dict = open('jan10dd.txt').read()
        #elif mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('may12dd.txt').read()
        ## data dictionary file for years 2010-apr2012
        #data_dict = open('jan10dd.txt').read()
        ## data dictionary file for year 2009
        #data_dict = open('jan09dd.txt').read()
        ## data dictionary file for years 2007-2008
        #data_dict = open('jan07dd.txt').read()
        ## data dictionary files for year 2006
        #data_dict = open('augnov05dd.txt').read()
        ## data dictionary files for year 2005
        #if mon in ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul']:
        #    data_dict = open('may04dd.txt').read()
        #elif mon in ['aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('augnov05dd.txt').read()
        ## data dictionary files for year 2004
        #if mon in ['jan', 'feb', 'mar', 'apr']:
        #    data_dict = open('jan03dd.txt').read()
        #elif mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('may04dd.txt').read()
        ## data dictionary files for year 2003
        #data_dict = open('jan03dd.txt').read()
        ## data dictionary files for years 1998-2002
        data_dict = open('jan98dd.asc.txt').read()
        age_var = age_var.upper()
        race_var = race_var.upper()
        earn_var = earn_var.upper()
        gend_var = gend_var.upper()
        var_names = [race_var, age_var, earn_var, gend_var]
        #print()
        #print(mon)
        #print(data_dict)
        if (yr in ['19','18','17','16','15','14','13']) or (yr == '12' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']):
            p = f'\n({"|".join(var_names)})\s+(\d+)\s+.*?\t+.*?(\d\d*).*?(\d\d+)'
            d = {s[0]: [int(s[2])-1, int(s[3]), f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #print(re.findall(p, data_dict))
            #print(p)
            #print(d)
        elif (yr in ['11','10','09','08','07','06','05','04','03']) or (yr == '12' and mon in ['jan', 'feb', 'mar', 'apr']):
            p = f'\n({"|".join(var_names)})\s+(\d+)\s+.*?\t*?(\d\d*).*?(\d\d+)'
            d = {s[0]: [int(s[2]) - 1, int(s[3]), f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #PESEX = f'\n({"PESEX"})\s+(\d+)\s+.*?\t+.*?(\d\d*).*?(\d\d+)'
            #print(re.findall(PESEX, data_dict))
            #print(p)
            #print(d)
        elif (yr in ['02','01','00']):
            #p = f'\n\S\s({"|".join(var_names)})\s+(\d\d*)\s+(\d\d*)\n'
            p = f'\n\S\s({"|".join([earn_var, race_var])})\s+(\d\d*)\s+(\d\d*)\n'
            #print(re.findall(p, data_dict))
            d = {s[0]: [int(s[2]) - 1, int(s[2])+int(s[1])-1, f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #print(p)
            print(d)

        start, end, width = zip(*d.values())
        skip = ([f'{s - e}x' for s, e in zip(start, [0] + list(end[:-1]))])
        unpack_fmt = ''.join([j for i in zip(skip, width) for j in i])
        #print(unpack_fmt)
        unpacker = struct.Struct(unpack_fmt).unpack_from

        ## files for years 2006-2019
        #raw_data = open('{0}{1}pub.dat'.format(mon, yr),'rb').readlines()
        ## files for year 2006
        #if mon in ['mar','apr','may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    raw_data = open('{0}{1}pub.dat'.format(mon, yr),'rb').readlines()
        #elif mon in ['jan','feb']:
        #    raw_data = open('{0}{1}pub.cps'.format(mon, yr), 'rb').readlines()
        ## files for years 2000-2005
        if (yr in ['02', '03', '04', '05']) or (yr == '01' and mon != 'mar') or (yr == '00' and mon in ['aug', 'dec', 'feb', 'jun', 'nov', 'oct', 'sep']):
            raw_data = open('{0}{1}pub.cps'.format(mon,yr), 'rb').readlines()
        elif (yr == '01' and mon == 'mar') or (yr == '00' and mon in ['jan', 'apr', 'jul', 'mar', 'may']):
            raw_data = open('{0}{1}pub.dat'.format(mon, yr), 'rb').readlines()
        #print(raw_data)
        race = d[race_var]
        #print(int(raw_data[0][race[0]:race[1]]))
        data = [[*map(int, unpacker(row))] for row in raw_data
                if int(row[race[0]:race[1]]) == aian_val]
        print(data[:5])

        df = pd.DataFrame(data, columns=d.keys())
        # get average age
        #avg_age = np.mean(df[age_var])
        #compiled_data.loc[count, 'Average Age'] = avg_age
        # get average earnings
        avg_earnings = np.mean(df[earn_var])
        compiled_data.loc[count, 'Average Weekly Earnings'] = avg_earnings
        # get gender distribution
        #male_prop = len(df[df[gend_var] == male_val]) / len(df)
        #female_prop = len(df[df[gend_var] == female_val]) / len(df)
        #compiled_data.loc[count, 'Male Proportion'] = male_prop
        #compiled_data.loc[count, 'Female Proportion'] = female_prop

        compiled_data.loc[count, 'Survey Size'] = len(df)

    ## data compilation format for years 2020-2022
    if yr in ['22', '21', '20']:
        filename = '{0}{1}pub.csv'.format(mon, yr)
        data = pd.read_csv(filename)
        data = data[data[race_var] == aian_val].reset_index(drop=True)
        # get average age
        avg_age = np.mean(data[age_var])
        compiled_data.loc[count, 'Average Age'] = avg_age
        # get average earnings
        avg_earnings = np.mean(data[earn_var])
        #compiled_data.loc[count, 'Average Weekly Earnings'] = avg_earnings
        compiled_data.loc[count, 'Average Hourly Earnings'] = avg_earnings
        # get gender distribution
        male_prop = len(data[data[gend_var] == male_val]) / len(data)
        female_prop = len(data[data[gend_var] == female_val]) / len(data)
        compiled_data.loc[count, 'Male Proportion'] = male_prop
        compiled_data.loc[count, 'Female Proportion'] = female_prop

        compiled_data.loc[count, 'Survey Size'] = len(data[earn_var])

    count += 1

    print(compiled_data)

#print(compiled_data)
compiled_data.to_csv('earnings.csv',index=False)



"""data = pd.read_csv('compiled-data.csv')
print(data)
data_reversed = data.iloc[::-1].replace(0, np.nan).reset_index(drop=True)
print(data_reversed)

data_reversed.to_csv('aian-avg_age-wkly_earnings-male_female_prop-num_surveyed.csv',index=False)"""