

// TypeScript declarations for @stdlib/lapack/base/dstebz

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection
	*/
	(
		range: string,
		order: string,
		N: number,
		vl: number,
		vu: number,
		il: number,
		iu: number,
		abstol: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		M: number,
		nsplit: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		IBLOCK: Int32Array,
		strideIBLOCK: number,
		offsetIBLOCK: number,
		ISPLIT: Int32Array,
		strideISPLIT: number,
		offsetISPLIT: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection
*/
declare var dstebz: Routine;

export = dstebz;
