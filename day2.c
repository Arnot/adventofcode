#include <stdio.h>
#include <math.h>

int main(int argc, char** argv)
{
        int l, w, h;
        int total, temp_sum;

        total = 0;

        while (scanf(" %dx%dx%d", &l, &w, &h) == 3) {
                temp_sum  = 2*l*w + 2*w*h + 2*h*l; 
                temp_sum += min(min(l*w, w*h), h*l);

                total += temp_sum;
        }

        printf("Total size: %d\n", total);

        return 0;
}
