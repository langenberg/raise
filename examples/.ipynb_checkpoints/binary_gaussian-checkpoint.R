library(raise)

set_api_key("sk-proj-P7nUJmwtobUijXk0oUyuT3BlbkFJZt6h2RJxBYrvB8ltdzsF")

# 1. study description and design -----------------------------------------

description <- '
The course is intended for third semester students in the bachelor program "Global Studies", and the semester has the common theme "migration and citizenship". This course provides students with an overview of the complexities of contemporary mobility trends and their legal, psychological, and social consequences for individual/group identities, global citizenship, and inclusion. This course encourages students to take two perspectives in understanding human movement: the analytical and the experiential.

On the analytical level, the course will address the core concepts and theories related to migration and citizenship, including legal categories and channels for mobility and the individual motivations for mobility. Within the course, students will also explore challenges and opportunities arising during migration processes and as a consequence of non-mobility. Furthermore, students will learn about psychological, legal and societal consequences of such migration processes. Questions of identity, belonging, connectedness to communities, inclusion/exclusion, integration, and eventual (transnational) citizenship will be explored through qualitative and quantitative data.

On the experiential level, students will be encouraged to explore different facets of migration through interrogation of their own and others’ migration trajectories and aspirations. Students will have the opportunity to define and explore a research question related to meso- and macro-level migration structures (e.g., policies and programmes at global, regional, national, and local level) and how those structures shape individual (micro-level) migration opportunities, experiences, and outcomes. In the semester project, students will confront how inequality is entrenched in global migration systems by answering the question “What is the power of my passport?”. In examining the legal dimensions of nationality and citizenship, students also explore how the constraints or opportunities leant by legal systems interact with personal mobility aspirations and intentions.

Students will also get acquainted with a framework, Intervention Mapping, to design needs-based interventions that will develop their skills relevant for the semester project, as well as future intervention approaches in the upcoming semesters. Intervention development, on an individual or policy level, is a complex endeavor and requires not only topical knowledge, but also structural and conceptual skills.
'

n_categorical <- c(3) # vector of levels each representing the number of levels of separate categorical variables
n_binary <- 1
n_continuous <- 1

# 2. binary outcome -------------------------------------------------------

binary_outcome <- StudyDesignBinary$new()$generate(
    description = description,
    n_continuous = n_continuous,
    n_categorical = n_categorical,
    n_binary = n_binary
)

binary_generator <- DataGenerator$new(binary_outcome)
dat_binary <- binary_generator$sample(n = 5000, empirical = T)

study_summary <- StudySummaryBinary$new(study_design = binary_outcome, data = dat_binary)
study_summary$render(output_file = "summary_binary.html", output_dir = "~/Downloads")


# 3. continuous outcome ---------------------------------------------------

continuous_outcome <- StudyDesignGaussian$new()$generate(
    description = description,
    n_continuous = n_continuous,
    n_categorical = n_categorical,
    n_binary = n_binary
)

continuous_generator <- DataGenerator$new(continuous_outcome)
dat_gaussian <- continuous_generator$sample(n = 200, empirical = F)

study_summary <- StudySummaryGaussian$new(study_design = continuous_outcome, data = dat_gaussian)
study_summary$render(output_file = "summary_gaussian.html", output_dir = "~/Downloads")

