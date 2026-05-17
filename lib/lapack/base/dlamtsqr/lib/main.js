
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlamtsqr = require( './dlamtsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlamtsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlamtsqr;
