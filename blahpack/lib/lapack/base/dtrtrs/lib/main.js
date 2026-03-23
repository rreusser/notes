

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dtrtrs = require( './dtrtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrtrs;
