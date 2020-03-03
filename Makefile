##
## EPITECH PROJECT, 2020
## FUN_wolfram_2019
## File description:
## Makefile
##

NAME	= wolfram

CC		= ghc

RM		= rm -f

SRCS	= Main.hs

all: $(NAME)

$(NAME): $(OBJS)
		$(CC) -o $(NAME) $(SRCS)

clean:
	$(RM) *.o
	$(RM) *.hi

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re
