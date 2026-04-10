# Lambda+Calculus Compiler

@i_v1 4
@s_v2 "string"
@l_v3 4l
@l_v4 7l
@f_v5 2.4

~add x,y x + y
~sub x,y x - y
~mul x,y x * y
~div x,y x / y

#(
    main function
)

~(
    print (add 2 4)
    print (add 2 v1)
    print (add 2 3)
    print (add 6l 3l)
    print (add v3 v4)
    print (add (sub 5 2) 3)
    print (add 2 (sub 5 3))

    print (sub 2 3)
    print (sub (add 2 3) 1)

    print (mul 2 2)
    print (mul v3 v4)

    print (div 9 3)
    print (div 9 4)
    print (div v5 2)
    print (div v5 v5)

    print 2 + 100
    print 1 / 2
    print (div 5 0)
)

