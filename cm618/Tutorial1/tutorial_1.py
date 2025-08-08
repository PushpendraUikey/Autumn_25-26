import xarray as xr 
import matplotlib.pyplot as plt

ds = xr.open_dataset('tutorial_t.nc')
global_mean = ds.air 

temp = ds.air 

temp.plot(marker='o', label='Global Mean Temperature')
plt.title('Global Mean Temperature Over Time')
plt.xlabel('Year')
plt.ylabel('Temperature (C)')
plt.grid(True)
plt.legend()
plt.show()

baseline_1 = temp.sel(year=slice(1951, 1980))
baseline1_mean = baseline_1.mean()
anamoly1 = temp - baseline1_mean

plt.figure(figsize=(10, 6))
anamoly1.plot(marker='o', label='Anamoly from 1951-1980 Baseline')
plt.title('Global Mean Temperature Anomalies (1951-1980 Baseline)')
plt.xlabel('Year')
plt.ylabel('Anomaly (C)')
plt.grid(True)
plt.legend()
plt.show()
