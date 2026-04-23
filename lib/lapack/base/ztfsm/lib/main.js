
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztfsm = require( './ztfsm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztfsm, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztfsm;
