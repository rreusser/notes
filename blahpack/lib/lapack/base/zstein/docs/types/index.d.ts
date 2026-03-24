

// TypeScript declarations for @stdlib/lapack/base/zstein

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvectors of a real symmetric tridiagonal matrix by inverse iteration
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		M: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		IBLOCK: Int32Array,
		strideIBLOCK: number,
		offsetIBLOCK: number,
		ISPLIT: Int32Array,
		strideISPLIT: number,
		offsetISPLIT: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		IFAIL: Int32Array,
		strideIFAIL: number,
		offsetIFAIL: number
	): Float64Array;
}

/**
* Computes eigenvectors of a real symmetric tridiagonal matrix by inverse iteration
*/
declare var zstein: Routine;

export = zstein;
