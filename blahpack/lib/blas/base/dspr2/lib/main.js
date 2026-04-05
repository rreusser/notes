
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspr2 = require( './dspr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspr2;
