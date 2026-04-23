
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrsna = require( './ztrsna.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrsna, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrsna;
