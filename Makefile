##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Rush 1
##

NAME = pushswap_checker

SRC = main.hs

OBJ = ghc

all: $(NAME)

$(NAME): $(HS_FILES)
	$(OBJ) -o $(NAME) Main.hs

clean:
	rm -f $(NAME) *.o *.hi

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re