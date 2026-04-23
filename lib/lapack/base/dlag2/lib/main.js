'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlag2 = require( './dlag2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlag2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlag2;
