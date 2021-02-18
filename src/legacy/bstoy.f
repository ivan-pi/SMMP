        subroutine bstoy
     *                  (n,
     *                   ia, ja,        syma, a,
     *                   ib, jb, diagb,       b,
     *                   move)
c
            integer      ia(*), ja(*),        syma,
     *                   ib(*), jb(*), diagb,
     *                   move
c
            real         a(*), b(*)
c
c       create the yale data structures b from the
c       corresponding bank-smith data structures a
c
c       compute ib
c
        if (diagb.eq.1) then
            ib(1) = n + 2
            icor = 0
            if (move.eq.1) then
                lshift = 0
                if (syma.eq.0) lshift = ia(n+1) - ia(1)
                do 2 i = 1,n
    2               b(i) = a(i)
            endif
        else
            ib(1) = 1
            icor = 1
        endif
        do 10 i=1,n
   10       ib(i+1)=ia(i+1)-ia(i)+icor
        do 30 i=1,n
            do 20 j=ia(i),ia(i+1)-1
                ib(ja(j)+1)=ib(ja(j)+1)+1
   20       continue
   30   continue
c
        do 40 i=1,n
   40       ib(i+1)=ib(i+1)+ib(i)
        if (diagb.eq.0) then
            do 45 i = 1,n
                jb(ib(i)) = i
                if (move.eq.1) b(ib(i)) = a(i)
   45           ib(i) = ib(i) + 1
        endif
c
c       now compute jb
c
        do 60 i=1,n
            do 50 jj=ia(i),ia(i+1)-1
                j = ja(jj)
                jb(ib(j))=i
                jb(ib(i))=j
                if (move.eq.1) then
                    b(ib(j)) = a(jj)
                    b(ib(i)) = a(jj+lshift)
                endif
                ib(i)=ib(i)+1
                ib(j)=ib(j)+1
   50       continue
   60   continue
c
c       fixup ib
c
        do 70 i=n,2,-1
   70       ib(i)=ib(i-1)
        if (diagb.eq.1) then
            ib(1)=n+2
        else
            ib(1)=1
        endif
        return
        end