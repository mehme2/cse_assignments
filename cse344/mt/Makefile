default:
	@gcc -o BankServer -g -Wall -lrt -pthread bankServer.c teller.c util.c
	@gcc -o BankClient -g -Wall bankClient.c util.c

clean:
	@rm BankServer BankClient
