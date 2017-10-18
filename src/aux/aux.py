# -*- coding: utf-8 -*-


from questions import *
import harmony

questions_enabled=[True, True, True, True, True, True,True, True, True, True, True]
# questions_enabled=[False, #intervalos
#                 False, #acordes
#                 True,  #escala mayor
#                 False, #modos
#                 False, #areas tonales
#                 False, #dominantes secundarias
#                 False, #sustitución por tritonos
#                 False, #armaduras
#                 True, #acordes en escala menor
#                 False, #intercambio modal
#                 False] #notacion en analisis




def initialize_questions_to_be_asked():

    print ("Elige los tipos de preguntas")
    for question_type in aux.types:
        print ("Quieres preguntas sobre " + question_type[0] + "? (y/n)")
        inp = raw_input()   # wait for user
        if inp != "y":
            questions_enabled[question_type[1]] = False
            print ("Tu eleccion: NO")
        else:
            print ("Tu eleccion: SI")

    print ("Hecho")



def questions_to_be_asked(question_type):
    return questions_enabled[question_type]



def random_question():
    question = random.choice(questions_clasification)
    if not questions_to_be_asked(question[1]):
        return random_question()
    return question[0]



def print_name(root_note, alteration_root_note, ascending=True):
    if (root_note == None) or (alteration_root_note==None):
        return "ERROR PRINTING NAME, NOTE DOES NOT EXIST"
    return  harmony.notas[root_note] + harmony.alteraciones_notas[alteration_root_note][0]



def integer_to_roman(number):
    if number==0:
        return 0
    elif number==1:
        return "I"
    elif number==2:
        return "II"
    elif number==3:
        return "III"
    elif number==4:
        return "IV"
    elif number==5:
        return "V"
    elif number==6:
        return "VI"
    elif number==7:
        return "VII"


def ask_question (question, params):
    solution = question(*params)
    inp = raw_input()   # wait for user
    print("Solucion: " + solution)

def return_something_for_question(thing):
    return thing
