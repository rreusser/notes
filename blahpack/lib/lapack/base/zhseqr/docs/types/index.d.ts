

// TypeScript declarations for @stdlib/lapack/base/zhseqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues and Schur form of complex upper Hessenberg matrix
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
		lwork: number
	): Float64Array;
}

/**
* Compute eigenvalues and Schur form of complex upper Hessenberg matrix
*/
declare var zhseqr: Routine;

export = zhseqr;
