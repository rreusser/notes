

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlasr = require( './zlasr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlasr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlasr;
