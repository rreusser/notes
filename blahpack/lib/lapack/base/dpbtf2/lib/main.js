

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpbtf2 = require( './dpbtf2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbtf2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbtf2;
