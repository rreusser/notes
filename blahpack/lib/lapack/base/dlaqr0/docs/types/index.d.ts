

// TypeScript declarations for @stdlib/lapack/base/dlaqr0

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and Schur form using multishift QR with aggressive early deflation
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
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
		iloz: number,
		ihiz: number,
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
* Computes eigenvalues and Schur form using multishift QR with aggressive early deflation
*/
declare var dlaqr0: Routine;

export = dlaqr0;
