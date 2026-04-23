
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgsna = require( './ztgsna.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgsna, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgsna;
