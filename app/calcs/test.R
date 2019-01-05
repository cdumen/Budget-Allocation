# recalibrate alpha
# dt_cat1[, Alpha := (Current.Spend/Weeks) * (Count*weeks_covered)]

# # recalibrate beta
# dt_cat1[, Beta := Current.Return / (1-exp(-Current.Spend/Alpha))]

print(dt)