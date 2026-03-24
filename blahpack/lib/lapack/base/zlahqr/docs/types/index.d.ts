

// TypeScript declarations for @stdlib/lapack/base/zlahqr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues and Schur form of upper Hessenberg matrix
	*/
	(
		wantt: boolean,
		wantz: boolean,
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
		iloz: number,
		ihiz: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number
	): Float64Array;
}

/**
* Compute eigenvalues and Schur form of upper Hessenberg matrix
*/
declare var zlahqr: Routine;

export = zlahqr;
