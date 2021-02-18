        subroutine transp
     *                   (n, m,
     *                    ia, ja, diaga, a,
     *                    ib, jb,        b,
     *                    move)
c
            integer       ia(*), ja(*), diaga,
     *                    ib(*), jb(*),
     *                    move
c
            real          a(*), b(*)
c
c       compute b = a(transpose)
c
c       first make ib
c
        do 10 i=1,m+1
   10       ib(i)=0
        if (move.eq.1) then
            do 15 i =1,m+1
   15           b(i) = 0.
        endif
        if (diaga.eq.1) then
            ib(1)=m + 2
        else
            ib(1)=1
        endif
c
c       count indices for each column
c
        do 30 i=1,n
            do 20 j=ia(i),ia(i+1)-1
                ib(ja(j)+1)=ib(ja(j)+1)+1
   20       continue
   30   continue
        do 40 i=1,m
   40      ib(i+1)=ib(i)+ib(i+1)
c
c       now make jb
c
        do 60 i=1,n
            do 50 j=ia(i),ia(i+1)-1
                index=ja(j)
                jb(ib(index))=i
                if (move.eq.1) b(ib(index)) = a(j)
                ib(index)=ib(index)+1
   50       continue
   60   continue
c
c       now fixup ib
c
        do 70 i=m,2,-1
   70       ib(i)=ib(i-1)
        if (diaga.eq.1) then
            if (move.eq.1) then
                j = min(n,m)
                do 80 i = 1,j
   80               b(i) = a(i)
            endif
            ib(1)=m + 2
        else
            ib(1)=1
        endif
        return
        end