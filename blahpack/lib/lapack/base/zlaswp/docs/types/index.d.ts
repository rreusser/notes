// TypeScript declarations for @stdlib/lapack/base/zlaswp

interface Routine {
	(
		N: number,
		A: any,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		k1: number,
		k2: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		incx: number
	): any;
}

declare var zlaswp: Routine;

export = zlaswp;
