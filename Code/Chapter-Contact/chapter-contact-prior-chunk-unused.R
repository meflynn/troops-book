

PRIOR.T <- c(# Personal Contact
  set_prior("normal(0.60, 0.20)", class = "b", coef = "contact_persYes", dpar = "mupos"),
  set_prior("normal(0.25, 0.10)", class = "b", coef = "contact_persYes", dpar = "muneg"),
  set_prior("normal(-0.70, 0.50)", class = "b", coef = "contact_persYes", dpar = "mudk"),
  set_prior("normal(-0.40, 0.25)", class = "b", coef = "contact_persDontknowDdeclinetoanswer", dpar = "mupos"),
  set_prior("normal(-0.90, 0.30)", class = "b", coef = "contact_persDontknowDdeclinetoanswer", dpar = "muneg"),
  set_prior("normal(0.55, 0.60)", class = "b", coef = "contact_persDontknowDdeclinetoanswer", dpar = "mudk"),
  # Network contact
  set_prior("normal(0.20, 0.10)", class = "b", coef = "contact_nonpersYes", dpar = "mupos"),
  set_prior("normal(0.15, 0.1)", class = "b", coef = "contact_nonpersYes", dpar = "muneg"),
  set_prior("normal(-0.66, 0.40)", class = "b", coef = "contact_nonpersYes", dpar = "mudk"),
  set_prior("normal(0.13, 0.07)", class = "b", coef = "contact_nonpersDontknowDdeclinetoanswer", dpar = "mupos"),
  set_prior("normal(0.04, 0.10)", class = "b", coef = "contact_nonpersDontknowDdeclinetoanswer", dpar = "muneg"),
  set_prior("normal(-0.70, 0.50)", class = "b", coef = "contact_nonpersDontknowDdeclinetoanswer", dpar = "mudk"),
  # Personal Benefit
  set_prior("normal(-0.09, 0.15)", class = "b", coef = "benefit_persYes", dpar = "mupos"),
  set_prior("normal(-0.40, 0.3)", class = "b", coef = "benefit_persYes", dpar = "muneg"),
  set_prior("normal(-0.40, 0.60)", class = "b", coef = "benefit_persYes", dpar = "mudk"),
  set_prior("normal(-0.25, 0.2)", class = "b", coef = "benefit_persDontknowDdeclinetoanswer", dpar = "mupos"),
  set_prior("normal(-0.30, 0.20)", class = "b", coef = "benefit_persDontknowDdeclinetoanswer", dpar = "muneg"),
  set_prior("normal(0.45, 0.30)", class = "b", coef = "benefit_persDontknowDdeclinetoanswer", dpar = "mudk"),
  # Network Benefit
  set_prior("normal(0.50, 0.1)", class = "b", coef = "benefit_nonpersYes", dpar = "mupos"),
  set_prior("normal(-0.45, 0.3)", class = "b", coef = "benefit_nonpersYes", dpar = "muneg"),
  set_prior("normal(-0.45, 0.50)", class = "b", coef = "benefit_nonpersYes", dpar = "mudk"),
  set_prior("normal(-0.2, 0.15)", class = "b", coef = "benefit_nonpersDontknowDdeclinetoanswer", dpar = "mupos"),
  set_prior("normal(-0.40, 0.30)", class = "b", coef = "benefit_nonpersDontknowDdeclinetoanswer", dpar = "muneg"),
  set_prior("normal(0.3, 0.20)", class = "b", coef = "benefit_nonpersDontknowDdeclinetoanswer", dpar = "mudk"),
  # Religion
  set_prior("normal(0.06, 0.25)", class = "b", coef = "religBuddhism", dpar = "mupos"),
  set_prior("normal(0.12, 0.15)", class = "b", coef = "religCatholicism", dpar = "mupos"),
  set_prior("normal(0.03, 0.15)", class = "b", coef = "religChristianityprotestant", dpar = "mupos"),
  set_prior("normal(0.20, 0.10)", class = "b", coef = "religDeclinetoanswer", dpar = "mupos"),
  set_prior("normal(-0.16, 0.30)", class = "b", coef = "religHinduism", dpar = "mupos"),
  set_prior("normal(-0.15, 0.12)", class = "b", coef = "religIslam", dpar = "mupos"),
  set_prior("normal(-0.12, 0.25)", class = "b", coef = "religJudaism", dpar = "mupos"),
  set_prior("normal(0.06, 0.25)", class = "b", coef = "religBuddhism", dpar = "mupos"),
  set_prior("normal(0.06, 0.25)", class = "b", coef = "religBuddhism", dpar = "mupos"),
  set_prior("normal(0.06, 0.25)", class = "b", coef = "religBuddhism", dpar = "mupos"),
  set_prior("normal(0.06, 0.25)", class = "b", coef = "religBuddhism", dpar = "mupos")
)

