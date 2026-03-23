

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasr = require( './dlasr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasr;
