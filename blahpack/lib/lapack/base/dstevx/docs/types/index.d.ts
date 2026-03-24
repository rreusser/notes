

// TypeScript declarations for @stdlib/lapack/base/dstevx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues and eigenvectors of a real symmetric tridiagonal matrix
	*/
	(
		jobz: string,
		range: string,
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		vl: number,
		vu: number,
		il: number,
		iu: number,
		abstol: number,
		M: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
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
* Computes selected eigenvalues and eigenvectors of a real symmetric tridiagonal matrix
*/
declare var dstevx: Routine;

export = dstevx;
