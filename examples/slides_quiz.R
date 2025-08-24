library(raise)

power_up()

slides_quiz <- QuizSlides$new(title = "Linear and Logistic Regression")
slides_quiz$upload_files("/Users/p70089410/Downloads/GLO2221 - Methods - Lecture 7_2.pdf")

slides_quiz$add_questions(
    question_mc(n_choices = 4, n_correct = 1, n_questions = 5),
    question_numeric(n_questions = 5)
)

slides_quiz$summary(
    output_file = "summary_quiz.html", 
    output_dir = "~/Downloads"
)

slides_quiz$export(
    output_file = "canvas_quiz_new_new",
    destination = "canvas",
    output_dir = "~/Downloads",
    verbose = T
)

slides_quiz$cover_tracks()

