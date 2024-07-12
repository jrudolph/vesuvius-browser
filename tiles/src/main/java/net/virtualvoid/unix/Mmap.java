package net.virtualvoid.unix;

import com.sun.jna.Library;
import com.sun.jna.Native;

public interface Mmap extends Library {
    Mmap INSTANCE = Native.load("c", Mmap.class);

    int MADV_NORMAL = 0;
    int MADV_RANDOM = 1;
    int MADV_SEQUENTIAL = 2;
    int MADV_WILLNEED = 3;
    int MADV_DONTNEED = 6;


    int madvise(long addr, long length, int advice);
}