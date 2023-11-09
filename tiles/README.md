# Vesuvius tiles server

The idea is to provide dynamic and scalable access to volume data.

Tiles are defined as 128x128x128 blocks which contains ~2M voxels. This is the atomic unit of data transfer.
However, tiles are provided in a pyramid structure with different quality levels that allow to progressively
receive data of higher quality.

## Quality pyramid

There are two basic dimensions of quality:
 * bit depth of data (original data is 16 bit but any smaller amount of bits is still useful, even 1-bit)
 * spatial resolution

To allow progressive transfer of data, the quality levels are required to "add up" to the next level up to the
point where the original data is available in full quality, i.e. we do not do lossy compression.

To provide progressive bit depth levels, we provide certain bit planes as separate layers:

 * MSB: 1-bit depth
 * + next significant bit: 2-bit depth
 + + next 2 significant bits: 4-bit depth
 + + next 4 significant bits: 8-bit depth
 + + next 8 significant bits: 16-bit depth

Spatial resolution:

Here it does not really make sense to provide the data progressively since there's a factor 2^3 = 8 size difference
between each level, so that we can only save 1/8th of data of providing the level progressively over the previous level.

 * every 8th, 4th, 2th, 1th voxel (with a factor of 2^3 size difference of each step) 

Bytes per block

| Bits/Res | 1/8  | 1/4   | 1/2    | 1/1     |
|----------|------|-------|--------|---------|
| 1        | 512  | 4096  | 32768  | 262144  | 
| 2        | 1024 | 8192  | 65536  | 524288  |
| 4        | 2048 | 16384 | 131072 | 1048576 |
| 8        | 4096 | 32768 | 262144 | 2097152 |
| 16       | 8192 | 65536 | 524288 | 4194304 |

That means to show a 512x512x512 cross cut in three dimensions, we need to transfer 16 blocks per dimension, i.e. 48 blocks in total or ~100MB to show the picture (i.e. 8 seconds of loading time at 100MBit/s if not cached).