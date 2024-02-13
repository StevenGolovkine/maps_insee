################################################################################
## Logements ordinaires
################################################################################

# Load pakcages
using CSV, DataFrames

# Load data
df = CSV.read("./data/FD_LOGEMT_2020.csv", DataFrame;
    select=["COMMUNE"], types=String, delim=";"
);

# Groupby COMMUNE
df_commune = groupby(df, :COMMUNE);
df_commune = combine(df_commune, nrow => :N_COMMUNE);

# Count the number of LOGEMENT
df_region = transform(
    df_commune, :COMMUNE => ByRow(
        x -> ifelse(startswith(x, "97"), SubString(x, 1, 3), SubString(x, 1, 2))
    ) => :REGION
);
df_region = groupby(df_region, :REGION);
df_region = combine(df_region, :N_COMMUNE => sum => :N_LOGEMENT);

# Save result
CSV.write("./data/logements.csv", df_region);