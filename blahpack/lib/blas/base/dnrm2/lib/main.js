

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dnrm2 = require( './dnrm2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dnrm2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dnrm2;
