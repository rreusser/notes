
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsymm = require( './dsymm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsymm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsymm;
