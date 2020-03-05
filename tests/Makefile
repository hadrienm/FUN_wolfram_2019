##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

NAME	= unit_tests

CC	= ghc

RM	= rm -f

SRCS	= Test.hs

all: $(NAME) run

$(NAME): $(OBJS)
	 $(CC) -o $(NAME) $(SRCS)

clean:
	$(RM) *.o
	$(RM) *.hi

fclean: clean
	$(RM) $(NAME)

run:		$(NAME)
			./unit_tests

re: fclean all

.PHONY: all clean fclean re tests_run
