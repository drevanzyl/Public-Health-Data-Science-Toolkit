import random

civics_questions = [
    {
        "question": "Name one writer of the Federalist Papers.",
        "answer": ["James Madison", "Alexander Hamilton", "John Jay", "Publius"],
        "explanation": "The Federalist Papers were written by James Madison, Alexander Hamilton, and John Jay under the pseudonym Publius."
    },
    {
        "question": "How many amendments does the Constitution have?",
        "answer": ["27", "twenty-seven"],
        "explanation": "The Constitution has been amended 27 times since its ratification."
    },
    {
        "question": "How many voting members are in the House of Representatives?",
        "answer": ["435", "four hundred thirty-five"],
        "explanation": "The House of Representatives has 435 voting members, with the number of representatives per state determined by the state's population."
    },
    {
        "question": "When was the Constitution written?",
        "answer": ["1787"],
        "explanation": "The Constitution was written in 1787 during the Constitutional Convention in Philadelphia."
    },
    {
        "question": "How many Justices are on the Supreme Court?",
        "answer": ["9", "nine"],
        "explanation": "The Supreme Court of the United States consists of nine justices."
    },
    {
        "question": "Who is the Chief Justice of the United States now?",
        "answer": ["John Roberts", "John G. Roberts", "John G. Roberts, Jr."],
        "explanation": "John G. Roberts, Jr. is the current Chief Justice of the United States Supreme Court."
    },
    {
        "question": "Who was President during World War I?",
        "answer": ["Woodrow Wilson"],
        "explanation": "Woodrow Wilson was the President of the United States during World War I, which lasted from 1914 to 1918."
    },
    {
        "question": "What is the rule of law?",
        "answer": ["Everyone must follow the law", "Leaders must obey the law", "Government must obey the law", "No one is above the law"],
        "explanation": "The rule of law means that everyone, including citizens, leaders, and the government, must follow and obey the law."
    },
    {
        "question": "Who was President during the Great Depression and World War II?",
        "answer": ["Franklin Roosevelt", "Franklin D. Roosevelt", "FDR"],
        "explanation": "Franklin D. Roosevelt (FDR) was President during both the Great Depression and World War II."
    },
    {
        "question": "Name one of the two longest rivers in the United States.",
        "answer": ["Missouri", "Mississippi"],
        "explanation": "The Missouri River and the Mississippi River are the two longest rivers in the United States."
    }
]

def ask_question(question):
    print("\n" + question["question"])
    user_answer = input("Your answer: ").strip()
    correct = any(ans.lower() == user_answer.lower() for ans in question["answer"])
    
    if correct:
        print("Correct!")
    else:
        print(f"Sorry, that's not correct. The correct answer could be: {' or '.join(question['answer'])}")
    
    print("Explanation:", question["explanation"])
    return correct

def run_quiz():
    score = 0
    questions = random.sample(civics_questions, len(civics_questions))
    
    for question in questions:
        if ask_question(question):
            score += 1
    
    print(f"\nYou got {score} out of {len(questions)} questions correct.")
    percentage = (score / len(questions)) * 100
    print(f"Your score: {percentage:.2f}%")

    if percentage == 100:
        print("Congratulations! You've mastered these civics questions!")
    elif percentage >= 80:
        print("Great job! You're well on your way to mastering these civics questions.")
    elif percentage >= 60:
        print("Good effort! Keep studying and you'll improve your score.")
    else:
        print("Keep practicing! Review the explanations and try again.")

if __name__ == "__main__":
    print("Welcome to the Civics Quiz App!")
    print("This quiz will test your knowledge of U.S. civics.")
    print("Type your answer and press Enter. Let's begin!")
    
    while True:
        run_quiz()
        play_again = input("\nWould you like to take the quiz again? (yes/no): ").lower()
        if play_again != 'yes':
            print("Thank you for using the Civics Quiz App. Goodbye!")
            break
