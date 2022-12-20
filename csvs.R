# Auxiliary engine and boiler power output, by ship type, size and operational mode
# GHG4 P84 Table 17
AE_B_Power=fread("/Users/mizexin/Desktop/Auxiliary_engine_boiler_power_output.csv")

# The SFCbase given in g/kWh for different engine and fuel types, and year of built
# GHG4 P88 Tabel 19
SFC_base=fread("/Users/mizexin/Desktop/SFC_base.csv")

# Low load adjustment factors used
# GHG4 P92 Tabel 20
Low_load_adjustment_factors=fread("/Users/mizexin/Desktop/Low_load_adjustment_factors.csv")

# Different fuels' fuel-based emission factors (EFf) and their carbon content.
# GHG4 P92 Table 21
EFf&Carbon_Content=fread("/Users/mizexin/Desktop/EFf&Carbon_Content.csv")

# Main engine power correction factors due to weather and fouling
# GHG4 P153 Appendix L
Correction_Factors=fread("/Users/mizexin/Desktop/Main_engine_power_correction_factors_due_to_weather_and_fouling.csv")

=fread("/Users/mizexin/Desktop/")
=fread("/Users/mizexin/Desktop/")
=fread("/Users/mizexin/Desktop/")