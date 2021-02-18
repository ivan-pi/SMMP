        subroutine ytobs
     *                  (n,
     *                   ia, ja, diaga, syma, a,
     *                   ib, jb,              b,
     *                   move)
c
            integer     ia(*), ja(*), diaga, syma,
     *                  ib(*), jb(*), move
c
            real        a(*), b(*)
c
c       create the bank-smith data structures b from the
c       corresponding yale data structures a
c
        do 10 i=1,n
   10       ib(i+1)=ia(i+1)-ia(i)
c
c       look for upper triangular entries and duplicate entries
c
        do 50 i=1,n
            do 40 jj=ia(i),ia(i+1)-1
                j=ja(jj)
                if (i.eq.j) then
                    ib(i+1)=ib(i+1)-1
                    ja(jj) = -j
                endif
                if(j.gt.i) then
                    ib(i+1)=ib(i+1)-1
                    ib(j+1)=ib(j+1)+1
c
c       check for duplicates
c
                    do 20 k=ia(j),ia(j+1)-1
                        if(ja(k).eq.i) then
                            ib(j+1)=ib(j+1)-1
                            ja(jj)=-j
                            go to 30
                        endif
   20               continue
   30               continue
                endif
   40       continue
   50   continue
c
c       compute ib
c
        ib(1)=n + 2
        do 60 i=1,n
   60       ib(i+1)=ib(i+1)+ib(i)
c
c       initialize b if move = 1
c
        if (move.eq.1) then
            lshift = 0
            if (syma.eq.0) lshift = ib(n+1) - ib(1)
            do 62 ii = 1,ib(n+1)+lshift-1
   62           b(ii) = 0.
            if (diaga.eq.1) then
                do 64 ii = 1,n
   64               b(ii) = a(ii)
            endif
        endif
c
c       compute jb
c
        do 80 i=1,n
            do 70 jj=ia(i),ia(i+1)-1
                j=ja(jj)
                if(j.gt.i) then
                    jb(ib(j))=i
                    if (move.eq.1) b(ib(j)) = a(jj)
                    ib(j)=ib(j)+1
                else
                    if(j.le.0) then
                        ja(jj)=-j
                        if (move.eq.1 .and. i.eq.-j) b(i) = a(jj)
                    else
                        jb(ib(i))=j
                        if (move.eq.1) b(ib(i)+lshift) = a(jj)
                        ib(i)=ib(i)+1
                    endif
                endif
   70       continue
   80   continue
c
c       fixup ib
c
        do 90 i=n,2,-1
   90       ib(i)=ib(i-1)
        ib(1)=n + 2
        return
        end