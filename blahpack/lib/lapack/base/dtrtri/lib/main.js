

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrtri = require( './dtrtri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrtri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrtri;
