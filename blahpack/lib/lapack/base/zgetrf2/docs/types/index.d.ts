// TypeScript declarations for @stdlib/lapack/base/zgetrf2

interface Routine {
	(
		M: number,
		N: number,
		A: any,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): number;
}

declare var zgetrf2: Routine;

export = zgetrf2;
