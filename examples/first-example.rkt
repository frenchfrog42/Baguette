; file to have an interactive repl

(require "../compilation.rkt")
(require "../vyper.rkt")
(require "../hashtable.rkt")

(define test-contract
  '(public (premier deuxieme)
    (= (destroy premier) (destroy deuxieme))))

(printf "~nHello~nOne example of contract is '~a~n" test-contract)
(printf "To compile it, you execute (contract->opcodes <<your contract>>)~n")
(printf "So here it's (contract->opcodes test-contract). Here is what is does:~n~n")

(println (contract->opcodes test-contract))

(printf "~nYou can now type in the repl any (contract->opcodes whatever) command to compile code !~nPlease try (contract->opcodes '(+ 1 2))~n")
(printf "Please also try (contract->opcodes '(public (secret) (= (destroy secret) 12)))~n")
(printf "(To copy paste press Ctrl+Shift+C and Ctrl+Shift+V)~n~n")

(printf "For more information, please read our documentation: https://replit-docs.frenchfrog42.repl.co~n")
(printf "If you have any question, ask it on discord: https://discord.com/invite/GvjAdXTMXz~n~n")