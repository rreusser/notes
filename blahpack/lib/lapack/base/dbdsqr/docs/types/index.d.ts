

// TypeScript declarations for @stdlib/lapack/base/dbdsqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the SVD of a real bidiagonal matrix
	*/
	(
		uplo: string,
		N: number,
		ncvt: number,
		nru: number,
		ncc: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		VT: Float64Array,
		strideVT1: number,
		strideVT2: number,
		offsetVT: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the SVD of a real bidiagonal matrix
*/
declare var dbdsqr: Routine;

export = dbdsqr;
