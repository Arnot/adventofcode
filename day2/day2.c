#include <stdio.h>
#include <math.h>

int main(int argc, char** argv)
{
        int l, w, h;
        int total_paper, temp_paper;
        int total_ribbon, temp_ribbon;

        total_paper = 0;
        total_ribbon = 0;

        while (scanf(" %dx%dx%d", &l, &w, &h) == 3) {
                temp_paper  = 2*l*w + 2*w*h + 2*h*l;
                temp_paper += fmin(fmin(l*w, w*h), h*l);

                temp_ribbon  = fmin(fmin(2*l+2*w, 2*w+2*h), 2*h+2*l);
                temp_ribbon += l*w*h;

                total_paper += temp_paper;
                total_ribbon += temp_ribbon;
        }

        printf("Total size of wrapping paper: %d\n", total_paper);
        printf("Total length of ribbon: %d\n", total_ribbon);

        return 0;
}
