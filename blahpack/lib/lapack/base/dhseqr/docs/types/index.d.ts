

// TypeScript declarations for @stdlib/lapack/base/dhseqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix
	*/
	(
		job: string,
		compz: string,
		N: number,
		ilo: number,
		ihi: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix
*/
declare var dhseqr: Routine;

export = dhseqr;
