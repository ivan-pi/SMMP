        subroutine numbmm
     *                   (n, m, l,
     *                    ia, ja, diaga, a,
     *                    ib, jb, diagb, b,
     *                    ic, jc, diagc, c,
     *                    temp)
c
            integer       ia(*), ja(*), diaga,
     *                    ib(*), jb(*), diagb,
     *                    ic(*), jc(*), diagc
c
            real          a(*), b(*), c(*), temp(*)
c
c       numeric matrix multiply c=a*b
c
        maxlmn = max(l,m,n)
        do 10 i = 1,maxlmn
 10         temp(i) = 0.
        minlm = min(l,m)
        minln = min(l,n)
        minmn = min(m,n)
c
c   c = a*b
c
        do 50 i = 1,n
             do 30 jj = ia(i),ia(i+1)
c    a = d + ...
                if (jj.eq.ia(i+1)) then
                    if (diaga.eq.0 .or. i.gt.minmn) goto 30
                    j = i
                    ajj = a(i)
                else
                    j=ja(jj)
                    ajj = a(jj)
                endif
c    b = d + ...
                if (diagb.eq.1 .and. j.le.minlm)
     *              temp(j) = temp(j) + ajj * b(j)
                do 20 k = ib(j),ib(j+1)-1
 20                 temp(jb(k)) = temp(jb(k)) + ajj * b(k)
 30         continue
c    c = d + ...
            if (diagc.eq.1 .and. i.le.minln) then
                c(i) = temp(i)
                temp(i) = 0.
            endif
            do 40 j = ic(i),ic(i+1)-1
                c(j) = temp(jc(j))
 40             temp(jc(j)) = 0.
 50     continue
        return
        end